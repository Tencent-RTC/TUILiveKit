//
//  UserStatusView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import TUICore
import Combine
import RTCRoomEngine
import RTCCommon

class UserStatusView: UIView {
    private let manager: LiveStreamManager
    private var cancellableSet = Set<AnyCancellable>()
    private var muteAudio: Bool = true
    private var isViewReady: Bool = false
    @Published private var userInfo: LSUser
    
    init(userInfo: LSUser, manager: LiveStreamManager) {
        self.userInfo = userInfo
        self.manager = manager
        super.init(frame: .zero)
        getUserInfo()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        subscribeUserInfo()
        backgroundColor = .g2.withAlphaComponent(0.4)
        layer.cornerRadius = 9
        layer.masksToBounds = true
    }
    
    private func getUserInfo() {
        Task {
            do {
                let info = try await manager.getUserInfo(userId: userInfo.userId)
                userInfo = LSUser(userInfo: info)
            } catch let err {
                debugPrint("getUserInfoError: \(err.localizedDescription)")
            }
        }
    }

    private lazy var userNameLabel: UILabel = {
        let user = UILabel()
        user.textColor = .white
        user.backgroundColor = UIColor.clear
        user.textAlignment = TUIGlobalization.getRTLOption() ? .right : .left
        user.numberOfLines = 1
        user.font = .customFont(ofSize: 9)
        addSubview(user)
        return user
    }()

    private lazy var voiceMuteImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_audio_mute_icon")
        return imageView
    }()
    
    private func constructViewHierarchy() {
        addSubview(userNameLabel)
        addSubview(voiceMuteImageView)
    }

    private func activateConstraints() {
        voiceMuteImageView.snp.remakeConstraints { make in
            make.leading.equalToSuperview().offset(8)
            make.width.height.equalTo(muteAudio ? 14 : 0)
            make.centerY.equalToSuperview()
        }

        userNameLabel.snp.remakeConstraints { make in
            make.leading.equalTo(voiceMuteImageView.snp.trailing).offset(muteAudio ? 2 : 0)
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().offset(-8)
        }
    }
    
    private func subscribeUserInfo() {
        $userInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] userInfo in
                guard let self = self else { return }
                if !userInfo.name.isEmpty {
                    userNameLabel.text = userInfo.name
                } else {
                    userNameLabel.text = userInfo.userId
                }
            }.store(in: &cancellableSet)
    }
    
    private func updateAudioStatus() {
        voiceMuteImageView.isHidden = !muteAudio
        activateConstraints()
    }
}

extension UserStatusView {
    
    func subscribeState() {
        manager.subscribeUserState(StateSelector(keyPath: \LSUserState.hasAudioStreamUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userIdList in
                guard let self = self else { return }
                if userIdList.contains(self.userInfo.userId) {
                    self.muteAudio = false
                } else {
                    self.muteAudio = true
                }
                self.updateAudioStatus()
            }
            .store(in: &cancellableSet)
    }
}

