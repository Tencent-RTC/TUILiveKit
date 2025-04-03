//
//  AudienceView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/19.
//

import Foundation
import RTCCommon
import Combine
import LiveStreamCore
import RTCRoomEngine

class AudienceView: RTCBaseView {
    let roomId: String
    // MARK: - private property
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private var cancellableSet: Set<AnyCancellable> = []
    
    // MARK: - property: view
    private let videoView: LiveCoreView
    private lazy var liveStreamObserver = LiveStreamObserver(manager: manager)
    private lazy var battleObserver = LSBattleManagerObserver(battleManager: manager.battleManager)
    
    lazy var livingView: AudienceLivingView = {
        let view = AudienceLivingView(manager: manager, routerManager: routerManager, coreView: videoView)
        return view
    }()
    
    lazy var dashboardView: AudienceEndView = {
        let roomOwner = manager.coreRoomState.ownerInfo
        let view = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.userName)
        view.delegate = self
        view.isHidden = true
        return view
    }()
    
    lazy var coverBgView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        let effect = UIBlurEffect(style: .light)
        let blurView = UIVisualEffectView(effect: effect)
        imageView.addSubview(blurView)
        blurView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        return imageView
    }()
    
    init(roomId: String, manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.videoView = coreView
        super.init(frame: .zero)
        self.videoView.videoViewDelegate = self
        self.videoView.registerConnectionObserver(observer: liveStreamObserver)
        self.videoView.registerBattleObserver(observer: battleObserver)
        self.manager.prepareRoomIdBeforeEnterRoom(roomId: roomId)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        videoView.unregisterConnectionObserver(observer: liveStreamObserver)
        videoView.unregisterBattleObserver(observer: battleObserver)
        print("deinit \(self)")
    }
    
    override func constructViewHierarchy() {
        backgroundColor = .black
        addSubview(coverBgView)
        addSubview(videoView)
        addSubview(livingView)
        addSubview(dashboardView)
    }
    
    override func activateConstraints() {
        coverBgView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        livingView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        dashboardView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        subscribeRoomState()
        subscribeMediaState()
        subscribeSubject()
    }
    
    func relayoutCoreView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        sendSubviewToBack(videoView)
        sendSubviewToBack(coverBgView)
    }
}

extension AudienceView {
    private func subscribeRoomState() {
        manager.subscribeState(StateSelector(keyPath: \LSRoomState.liveStatus))
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .finished:
                        routeToAudienceView()
                        showEndView()
                    default: break
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeCoreViewState(StateSelector(keyPath: \RoomState.ownerInfo.userName))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userName in
                guard let self = self, !userName.isEmpty else { return }
                dashboardView.update(userName: userName)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeCoreViewState(StateSelector(keyPath: \RoomState.ownerInfo.avatarUrl))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] avatarUrl in
                guard let self = self, !avatarUrl.isEmpty else { return }
                dashboardView.update(avatarUrl: avatarUrl)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \LSRoomState.coverURL))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] url in
                guard let self = self else { return }
                coverBgView.kf.setImage(with: URL(string: url))
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeMediaState() {
        manager.subscribeState(StateSelector(keyPath: \LSMediaState.isAudioLocked))
            .removeDuplicates()
            .dropFirst()
            .receive(on: RunLoop.main)
            .sink { [weak self] isAudioLocked in
                guard let self = self, manager.coGuestState.coGuestStatus == .linking else { return }
                manager.toastSubject.send(isAudioLocked ? .mutedAudioText : .unmutedAudioText)
            }
            .store(in: &cancellableSet)
        
        
        manager.subscribeState(StateSelector(keyPath: \LSMediaState.isVideoLocked))
            .removeDuplicates()
            .dropFirst()
            .receive(on: RunLoop.main)
            .sink { [weak self] isVideoLocked in
                guard let self = self, manager.coGuestState.coGuestStatus == .linking else { return }
                manager.toastSubject.send(isVideoLocked ? .mutedVideoText : .unmutedVideoText)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] coGuestStatus in
                guard let self = self else { return }
                manager.onCoGuestStatusChanged(status: coGuestStatus)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSubject() {
        manager.kickedOutSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                routeToAudienceView()
                onKickedByAdmin()
            }.store(in: &cancellableSet)
    }
    
    private func routeToAudienceView() {
        routerManager.router(action: .routeTo(.audience))
    }
    
    private func showEndView() {
        dashboardView.isHidden = false
    }
    
    private func onKickedByAdmin() {
        manager.toastSubject.send(.kickedOutText)
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
            guard let self = self else { return }
            routerManager.router(action: .exit)
        }
    }
}

extension AudienceView: LSRouterViewProvider {
    func getRouteView(route: LSRoute) -> UIView? {
        if route == .videoSetting {
            return VideoSettingPanel(routerManager: routerManager, manager: manager, coreView: videoView)
        }
        else {
            return nil
        }
    }
}

// MARK: - LiveEndViewDelegate
extension AudienceView: LiveEndViewDelegate {
    func onCloseButtonClick() {
        routerManager.router(action: .exit)
    }
}

extension AudienceView {
    func joinLiveStream() {
        videoView.joinLiveStream(roomId: roomId) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            manager.onJoinLive(roomInfo: roomInfo)
            livingView.initComponentView()
            livingView.isHidden = false
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
        }
    }
}

extension AudienceView: VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView? {
        return CoGuestView(userInfo: userInfo, manager: manager, routerManager: routerManager)
    }
    
    func updateCoGuestView(coGuestView: UIView, userInfo: TUIUserInfo, modifyFlag: LiveStreamCore.UserInfoModifyFlag) {
        
    }
    
    func createCoHostView(coHostUser: CoHostUser) -> UIView? {
        return CoHostView(connectionUser: coHostUser, manager: manager)
    }
    
    func updateCoHostView(coHostView: UIView, coHostUser: LiveStreamCore.CoHostUser, modifyFlag: LiveStreamCore.UserInfoModifyFlag) {
        
    }
    
    func createBattleView(battleUser: TUIBattleUser) -> UIView? {
        return BattleMemberInfoView(manager: manager, userId: battleUser.userId)
    }
    
    func updateBattleView(battleView: UIView, battleUser: TUIBattleUser) {
        
    }
    
    func createBattleContainerView() -> UIView? {
        return BattleInfoView(manager: manager, routerManager: routerManager, isOwner: true, coreView: videoView)
    }
    
    func updateBattleContainerView(battleContainerView: UIView, userInfos: [LiveStreamCore.BattleUserViewModel]) {
        if let battleInfoView = battleContainerView as? BattleInfoView {
            battleInfoView.updateView(userInfos: userInfos)
        }
    }
}

fileprivate extension String {
    static let kickedOutText = localized("You have been kicked out of the room by the anchor")
    static let mutedAudioText = localized("The anchor has muted you")
    static let unmutedAudioText = localized("The anchor has unmuted you")
    static let mutedVideoText = localized("The anchor disabled your video")
    static let unmutedVideoText = localized("The anchor enabled your video")
}
