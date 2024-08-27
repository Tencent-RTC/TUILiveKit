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
    var store: LiveStore
    private lazy var audioStreamPublisher = store.select(UserSelectors.getHasAudioStreamUserList)
    private var cancellableSet = Set<AnyCancellable>()
    private var muteAudio: Bool = true
    private var isViewReady: Bool = false
    private var userId: String = ""
    
    init(store: LiveStore) {
        self.store = store
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

    func updateUserStatus(_ renderModel: VideoRenderModel) {
        userId = renderModel.userId
        if !renderModel.userName.isEmpty {
            userNameLabel.text = renderModel.userName
        } else {
            userNameLabel.text = renderModel.userId
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
    var store: LiveStore
    lazy var videoStreamPublisher = store.select(UserSelectors.getHasVideoStreamUserList)
    var cancellableSet = Set<AnyCancellable>()
    private var isViewReady = false
    
    init(store: LiveStore) {
        self.store = store
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        cancellableSet.forEach { cancellable in
            cancellable.cancel()
        }
        cancellableSet.removeAll()
        print("deinit \(self)")
    }
    
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
    
    var renderModel: VideoRenderModel? {
        didSet {
            if oldValue?.userId != renderModel?.userId {
                updateView()
            }
        }
    }

    func updateView() {
        guard let renderModel = renderModel else { return }
        userInfoView.updateUserStatus(renderModel)
        avatarImageView.kf.setImage(with: URL(string: renderModel.avatarUrl), placeholder: UIImage.placeholderImage)
        updateAvatarImageView()
    }

    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        addSubview(imageView)
        return imageView
    }()

    lazy var userInfoView: UserStatusView = {
        let view = UserStatusView(store: store)
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
        guard let renderModel = renderModel else { return }
        if store.selectCurrent(UserSelectors.getHasVideoStreamUserList).contains(renderModel.userId) {
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
    private var viewPool: [String: RenderView] = [:]
    func getRenderView(_ renderModel: VideoRenderModel, store: LiveStore) -> RenderView {
        guard let view = viewPool[renderModel.userId] else {
            var view: RenderView
            if store.selectCurrent(UserSelectors.currentUserId) == renderModel.userId {
                view = StreamPublisherView(store: store)
            } else {
                view = StreamPlayerView(store: store)
            }
            view.renderModel = renderModel
            viewPool[renderModel.userId] = view
            return view
        }
        view.renderModel = renderModel
        return view
    }

    func removeRenderView(_ renderModel: VideoRenderModel) {
        viewPool.removeValue(forKey: renderModel.userId)
    }

    func removeRenderViews(_ renderModels: [VideoRenderModel]) {
        for renderModel in renderModels {
            removeRenderView(renderModel)
        }
    }
    
    func findRenderView(_ renderModel: VideoRenderModel) -> RenderView? {
        return viewPool[renderModel.userId]
    }
    
    func clearAllRenderView() {
        viewPool = [:]
    }
}
