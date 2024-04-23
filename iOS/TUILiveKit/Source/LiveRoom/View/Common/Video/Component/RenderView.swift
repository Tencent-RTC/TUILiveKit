//
//  RenderView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import TUICore
import Kingfisher

class UserStatusView: UIView {
    private var muteAudio: Bool = true
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        activateConstraints()
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

    func updateUserStatus(_ userInfo: UserInfo) {
        if !userInfo.name.value.isEmpty {
            userNameLabel.text = userInfo.name.value
        } else {
            userNameLabel.text = userInfo.userId
        }
        muteAudio = userInfo.audioInfo.muteAudio.value
        voiceMuteImageView.isHidden = !muteAudio
        activateConstraints()
    }
}

class RenderView: UIView {
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    var roomEngineService:RoomEngineService?
    var userInfo: UserInfo? {
        didSet {
            if oldValue?.userId != userInfo?.userId {
                updateView()
            }
        }
    }

    func updateView() {
        guard let userInfo = userInfo else { return }
        userInfo.videoInfo.isCameraOpened.addObserver(self) { [weak self] isCameraOpened, _ in
            self?.avatarImageView.isHidden = isCameraOpened
        }
        userInfo.audioInfo.muteAudio.addObserver(self) { [weak self] _, _ in
            guard let self = self else{ return}
            self.userInfoView.updateUserStatus(self.userInfo ?? userInfo)
        }
        userInfo.avatarUrl.addObserver(self) { [weak self] avatarUrl, _ in
            guard let self = self else{ return}
            self.avatarImageView.kf.setImage(with: URL(string: avatarUrl), placeholder: UIImage.placeholderImage)
        }
        
        userInfoView.updateUserStatus(userInfo)
        avatarImageView.kf.setImage(with: URL(string: userInfo.avatarUrl.value), placeholder: UIImage.placeholderImage)
        avatarImageView.isHidden = userInfo.videoInfo.isCameraOpened.value
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
}

class MatrixVideoRenderManager {
    private var viewPool: [String: RenderView] = [:]
    func getRenderView(_ userInfo: UserInfo,_ roomEngineService:RoomEngineService?) -> RenderView {
        guard let view = viewPool[userInfo.userId] else {
            var view: RenderView
            if TUILogin.getUserID() ?? "" == userInfo.userId {
                view = StreamPublisherView()
            } else {
                view = StreamPlayerView()
            }
            view.roomEngineService = roomEngineService
            view.userInfo = userInfo
            viewPool[userInfo.userId] = view
            return view
        }
        view.userInfo = userInfo
        return view
    }

    func removeRenderView(_ userInfo: UserInfo) {
        viewPool.removeValue(forKey: userInfo.userId)
    }

    func removeRenderViews(_ userInfos: [UserInfo]) {
        for userInfo in userInfos {
            removeRenderView(userInfo)
        }
    }
    
    func findRenderView(_ userInfo: UserInfo) -> RenderView? {
        return viewPool[userInfo.userId]
    }
    
    func clearAllRenderView() {
        viewPool = [:]
    }
}
