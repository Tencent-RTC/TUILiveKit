//
//  RenderView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import TUICore
import Kingfisher
import Combine

class UserStatusView: UIView {
    @Injected var store: LiveStore
    private lazy var audioStreamPublisher = store.select(UserSelectors.getHasAudioStreamUserList)
    private var cancellableSet = Set<AnyCancellable>()
    private var muteAudio: Bool = true
    private var isViewReady: Bool = false
    private var userId: String = ""
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        activateConstraints()
        subscribeSeatState()
        backgroundColor = .g2.withAlphaComponent(0.4)
        layer.cornerRadius = 9
        layer.masksToBounds = true
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
        addSubview(imageView)
        return imageView
    }()

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

    func updateUserStatus(_ seatInfo: SeatInfo) {
        userId = seatInfo.userId
        if !seatInfo.userName.isEmpty {
            userNameLabel.text = seatInfo.userName
        } else {
            userNameLabel.text = seatInfo.userId
        }
        if store.userState.hasAudioStreamUserList.first(where: { $0 == self.userId }) == nil {
            muteAudio = true
        } else {
            muteAudio = false
        }
    }
    
    private func updateAudioStatus() {
        voiceMuteImageView.isHidden = !muteAudio
        activateConstraints()
    }
}

extension UserStatusView {
    
    func subscribeSeatState() {
        audioStreamPublisher
            .receive(on: RunLoop.main)
            .filter { $0.count != 0 }
            .sink { [weak self] userIdList in
                guard let self = self else { return }
                if userIdList.first(where: { $0 == self.userId }) == nil {
                    self.muteAudio = true
                } else {
                    self.muteAudio = false
                }
                self.updateAudioStatus()
            }
            .store(in: &cancellableSet)
    }

}

class RenderView: UIView {
    @Injected var store: LiveStore
    lazy var videoStreamPublisher = store.select(UserSelectors.getHasVideoStreamUserList)
    var cancellableSet = Set<AnyCancellable>()
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeSeatState()
    }
    
    var seatInfo: SeatInfo? {
        didSet {
            if oldValue?.userId != seatInfo?.userId {
                updateView()
            }
        }
    }

    func updateView() {
        guard let seatInfo = seatInfo else { return }
        userInfoView.updateUserStatus(seatInfo)
        avatarImageView.kf.setImage(with: URL(string: seatInfo.avatarUrl), placeholder: UIImage.placeholderImage)
        updateAvatarImageView()
    }

    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        addSubview(imageView)
        return imageView
    }()

    let userInfoView: UserStatusView = {
        let view = UserStatusView()
        return view
    }()

    func constructViewHierarchy() {
        backgroundColor = .blackColor.withAlphaComponent(0.4)
        addSubview(avatarImageView)
        addSubview(userInfoView)
    }

    func activateConstraints() {
        avatarImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }

        userInfoView.snp.makeConstraints { make in
            make.height.equalTo(24)
            make.bottom.equalToSuperview().offset(-5)
            make.leading.equalToSuperview().offset(5)
            make.width.lessThanOrEqualTo(self).multipliedBy(0.9)
        }
    }
    
    private func updateAvatarImageView() {
        let currentLiveStatus = store.selectCurrent(ViewSelectors.getLiveStatus)
        if currentLiveStatus == .previewing {
            avatarImageView.isHidden = true
            return
        }
        guard let seatInfo = seatInfo else { return }
        if store.userState.hasVideoStreamUserList.contains(seatInfo.userId) {
            avatarImageView.isHidden = true
        } else {
            avatarImageView.isHidden = false
        }
    }
    
}

extension RenderView {
    
    private func subscribeSeatState() {
        videoStreamPublisher
            .receive(on: RunLoop.main)
            .filter { $0.count != 0 }
            .sink { [weak self] userIdList in
                guard let self = self else { return }
                self.updateAvatarImageView()
            }
            .store(in: &cancellableSet)
    }

}

class MatrixVideoRenderManager {
    @Injected var store: LiveStore
    private var viewPool: [String: RenderView] = [:]
    func getRenderView(_ seatInfo: SeatInfo) -> RenderView {
        guard let view = viewPool[seatInfo.userId] else {
            var view: RenderView
            if store.selectCurrent(UserSelectors.currentUserId) == seatInfo.userId {
                view = StreamPublisherView()
            } else {
                view = StreamPlayerView()
            }
            view.seatInfo = seatInfo
            viewPool[seatInfo.userId] = view
            return view
        }
        view.seatInfo = seatInfo
        return view
    }

    func removeRenderView(_ seatInfo: SeatInfo) {
        viewPool.removeValue(forKey: seatInfo.userId)
    }

    func removeRenderViews(_ seatInfos: [SeatInfo]) {
        for userInfo in seatInfos {
            removeRenderView(userInfo)
        }
    }
    
    func findRenderView(_ seatInfo: SeatInfo) -> RenderView? {
        return viewPool[seatInfo.userId]
    }
    
    func clearAllRenderView() {
        viewPool = [:]
    }
}
