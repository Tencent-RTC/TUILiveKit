//
//  CoHostView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine
import LiveStreamCore

class CoHostView: UIView {
    private let manager: LiveStreamManager
    private var isViewReady: Bool = false
    private var coHostUser: CoHostUser
    private var cancellableSet = Set<AnyCancellable>()
    
    init(connectionUser: CoHostUser, manager: LiveStreamManager) {
        self.coHostUser = connectionUser
        self.manager = manager
        super.init(frame: .zero)
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
        initViewState()
        subscribeState()
        self.isUserInteractionEnabled = false
    }
    
    private lazy var userInfoView = UserStatusView(userInfo: TUIUserInfo(coHostUser: coHostUser), manager: manager)
    
    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        imageView.isHidden = true
        return imageView
    }()
    
    private func constructViewHierarchy() {
        addSubview(userInfoView)
        addSubview(avatarImageView)
    }
    
    private func activateConstraints() {
        userInfoView.snp.makeConstraints { make in
            make.height.equalTo(18)
            make.bottom.equalToSuperview().offset(-5)
            make.leading.equalToSuperview().offset(5)
            make.width.lessThanOrEqualTo(self).multipliedBy(0.9)
        }
        avatarImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }
    }
    
    private func initViewState() {
        if coHostUser.connectionUser.userId == manager.coreUserState.selfInfo.userId {
            userInfoView.isHidden = true
        }
        avatarImageView.kf.setImage(with: URL(string: coHostUser.connectionUser.avatarUrl),
                                    placeholder: UIImage.avatarPlaceholderImage)
        let hasVideo = manager.coreUserState.hasVideoStreamUserList.contains(coHostUser.connectionUser.userId)
        avatarImageView.isHidden = hasVideo || coHostUser.hasVideoStream
    }
}

extension CoHostView {
    func subscribeState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \UserState.hasVideoStreamUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userIdList in
                guard let self = self else { return }
                if userIdList.contains(self.coHostUser.connectionUser.userId) || self.coHostUser.hasVideoStream {
                    avatarImageView.isHidden = true
                } else {
                    avatarImageView.isHidden = false
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                self.isHidden = connectedUsers.isEmpty
            }
            .store(in: &cancellableSet)
    }
}
