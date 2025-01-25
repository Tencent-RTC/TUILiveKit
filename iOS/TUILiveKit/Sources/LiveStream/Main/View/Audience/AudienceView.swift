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
        let roomOwner = manager.roomState.ownerInfo
        let view = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
        view.delegate = self
        view.isHidden = true
        return view
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
        addSubview(videoView)
        addSubview(livingView)
        addSubview(dashboardView)
    }
    
    override func activateConstraints() {
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
    }
    
    func relayoutCoreView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        sendSubviewToBack(videoView)
    }
}

extension AudienceView {
    private func subscribeRoomState() {
        manager.subscribeRoomState(StateSelector(keyPath: \LSRoomState.liveStatus))
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .none,.previewing:
                        // TODO: - mute all?
                        break
                    case .finished:
                        dashboardView.update(avatarUrl: manager.roomState.ownerInfo.avatarUrl,
                                             userName: manager.roomState.ownerInfo.name)
                        dashboardView.isHidden = false
                    case .playing:
                        self.didEnterRoom()
                        break
                    case .pushing:
                        break
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func didEnterRoom() {
        manager.fetchSeatList()
    }
}

extension AudienceView: LSRouterViewProvider {
    func getRouteView(route: LSRoute) -> UIView? {
        if route == .videoSetting {
            return VideoSettingPanel(routerManager: routerManager, mediaManager: videoView.mediaManager)
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
            manager.updateRoomState(roomInfo: roomInfo)
            manager.updateOwnerUserInfo()
            manager.update(liveStatus: .playing)
            livingView.initComponentView()
            livingView.isHidden = false
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
        }
    }
}

extension AudienceView: VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView? {
        return CoGuestView(userInfo: userInfo, manager: manager)
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

extension String {
    fileprivate static let enterRoomFailedMessageText = localized("live.alert.enterRoom.failed.message.xxx")
}
