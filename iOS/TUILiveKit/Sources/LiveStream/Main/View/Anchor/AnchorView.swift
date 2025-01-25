//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/14.
//

import Foundation
import Combine
import TUICore
import RTCRoomEngine
import LiveStreamCore
import RTCCommon

class AnchorView: UIView {
    private var isExited: Bool = false
    
    private let roomId: String
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    
    private lazy var coHostRequestPublisher = manager.coHostManager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.receivedConnectionRequest))
    private lazy var receivedBattleRequestPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.receivedBattleRequest))
    private lazy var isInWaitingPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isInWaiting))
    private var cancellableSet = Set<AnyCancellable>()
    var startLiveBlock:(()->Void)?
    
    private let videoView: LiveCoreView
    
    private lazy var prepareView: AnchorPrepareView = {
        let view = AnchorPrepareView(manager: manager, routerManager: routerManager)
        view.delegate = self
        return view
    }()
    
    private lazy var livingView: AnchorLivingView = {
        let view = AnchorLivingView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: videoView)
        view.alpha = 0
        return view
    }()
    
    private weak var alertPanel: LSAlertPanel?
    private lazy var liveStreamObserver = LiveStreamObserver(manager: manager)
    private lazy var battleObserver = LSBattleManagerObserver(battleManager: manager.battleManager)
    
    init(roomId: String, manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.videoView = coreView
        super.init(frame: .zero)
        self.videoView.videoViewDelegate = self
        self.videoView.registerConnectionObserver(observer: liveStreamObserver)
        self.videoView.registerBattleObserver(observer: battleObserver)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        videoView.stopCamera()
        videoView.stopMicrophone()
        videoView.unregisterConnectionObserver(observer: liveStreamObserver)
        videoView.unregisterBattleObserver(observer: battleObserver)
        print("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .black
        isViewReady = true
        initData()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        prepareView.updateRootViewOrientation(isPortrait: isPortrait)
        livingView.updateRootViewOrientation(isPortrait: isPortrait)
    }
    
    func initData() {
        manager.update(liveStatus: .previewing)
    }
    
    func relayoutCoreView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        sendSubviewToBack(videoView)
    }
}

extension AnchorView {
    
    private func constructViewHierarchy() {
        addSubview(videoView)
        addSubview(prepareView)
        addSubview(livingView)
    }
    
    private func activateConstraints() {
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        
        prepareView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        
        livingView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
    }
    
    private func bindInteraction() {
        videoView.startCamera(useFrontCamera: true) {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
        }
        videoView.startMicrophone() {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
        }
        subscribeCoHostState()
        subscribeBattleState()
    }
}

// MARK: Action

extension AnchorView {
    
    private func startLiving() {
        self.prepareView.alpha = 0
        UIView.animate(withDuration: 0.5) { [weak self] in
            guard let self = self else { return }
            self.livingView.alpha = 1
        } completion: { [weak self] _ in
            guard let self = self else { return }
            self.prepareView.removeFromSuperview()
        }
        startLiveBlock?()
    }
    
    private func createRoom() {
        startLiving()
        
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = roomId
        roomInfo.name = manager.roomState.roomName
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        roomInfo.seatMode = .applyToTake
        roomInfo.maxSeatCount = manager.roomState.maxSeatCount
        videoView.startLiveStream(roomInfo: roomInfo) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            handleAbnormalExitedSence()
            
            manager.update(liveStatus: .pushing)
            manager.updateRoomState(roomInfo: roomInfo)
            manager.syncLiveInfoToService()
            manager.fetchAudienceList()
            manager.updateOwnerUserInfo()
            manager.fetchSeatList()
            manager.updateSelfUserInfo()
            if manager.userState.selfInfo.role == .roomOwner {
                manager.fetchSeatApplicationList()
            }
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            manager.update(liveStatus: .none)
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
        }
    }
    
    func joinSelfCreatedRoom() {
        startLiving()
        videoView.joinLiveStream(roomId: roomId) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            manager.update(liveStatus: .pushing)
            manager.updateRoomState(roomInfo: roomInfo)
            manager.fetchAudienceList()
            manager.updateOwnerUserInfo()
            manager.fetchSeatList()
            manager.updateSelfUserInfo()
            if manager.userState.selfInfo.role == .roomOwner {
                manager.fetchSeatApplicationList()
            }
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            manager.update(liveStatus: .none)
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
        }
    }
    
    private func handleAbnormalExitedSence() {
        if isExited {
            videoView.stopLiveStream {
            } onError: { code, message in
            }
        }
    }
    
    private func subscribeCoHostState() {
        coHostRequestPublisher.receive(on: RunLoop.main)
            .sink { [weak self] connectionRequest in
                guard let self = self else { return }
                if !manager.coreCoGuestState.connectionRequestList.isEmpty {
                    // If received linkmic request first, reject connection auto.
                    if let request = connectionRequest {
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: false) { [weak self] in
                            guard let self = self else { return }
                            manager.coHostManager.onReject()
                        } onError: { _, _ in }
                    }
                    return
                }
                if let request = connectionRequest {
                    let alertInfo = LSAlertInfo(description: String.localizedReplace(.connectionInviteText, replace: "\(request.userName)"),
                                                imagePath: request.avatarUrl,
                                                cancelButtonInfo: (String.rejectText, .g3),
                                                defaultButtonInfo: (String.acceptText, .b1)) { [weak self] alertPanel in
                        guard let self = self else { return }
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: false) { [weak self] in
                            guard let self = self else { return }
                            manager.coHostManager.onReject()
                        } onError: { [weak self] err, msg in
                            guard let self = self else { return }
                            let error = InternalError(error: err, message: msg)
                            manager.toastSubject.send(error.localizedMessage)
                        }
                        alertPanel.dismiss()
                    } defaultClosure: { [weak self] alertPanel in
                        guard let self = self else { return }
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: true) { [weak self] in
                            guard let self = self else { return }
                            manager.coHostManager.onAccept()
                        } onError: { [weak self] err, msg in
                            guard let self = self else { return }
                            let error = InternalError(error: err, message: msg)
                            manager.toastSubject.send(error.localizedMessage)
                        }
                        alertPanel.dismiss()
                    }
                    let alertPanel = LSAlertPanel(alertInfo: alertInfo)
                    alertPanel.show()
                    self.alertPanel = alertPanel
                } else {
                    self.alertPanel?.dismiss()
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeBattleState() {
        receivedBattleRequestPublisher
            .removeDuplicates(by: {(firstRequest, secondRequest) -> Bool in
                return firstRequest?.battleId == secondRequest?.battleId &&
                firstRequest?.inviter.userId == secondRequest?.inviter.userId
            })
            .receive(on: RunLoop.main)
            .sink { [weak self] receivedRequest in
                guard let self = self else { return }
                self.onReceivedBattleRequestChanged(battleUser: receivedRequest?.inviter)
            }
            .store(in: &cancellableSet)
        
        isInWaitingPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] inWaiting in
                guard let self = self else { return }
                self.onInWaitingChanged(inWaiting: inWaiting)
            }
            .store(in: &cancellableSet)
    }
}

extension AnchorView {
    private func onReceivedBattleRequestChanged(battleUser: BattleUser?) {
        guard let battleUser = battleUser else {
            alertPanel?.dismiss()
            return
        }
        let alertInfo = LSAlertInfo(description: .localizedReplace(.battleInvitationText, replace: battleUser.userName),
                                    imagePath: battleUser.avatarUrl,
                                    cancelButtonInfo: (String.rejectText, .g3),
                                    defaultButtonInfo: (String.acceptText, .b1)) { [weak self] alertPanel in
            alertPanel.dismiss()
            guard let self = self else { return }
            videoView.respondToBattle(battleId: manager.battleState.battleId, isAccepted: false, onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.battleManager.onResponseBattle()
            }, onError: { _, _ in
                
            })
            
        } defaultClosure: { [weak self] alertPanel in
            alertPanel.dismiss()
            guard let self = self else { return }
            videoView.respondToBattle(battleId: manager.battleState.battleId, isAccepted: true, onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.battleManager.onResponseBattle()
            }, onError: { _, _ in
                
            })
        }
        let alertPanel = LSAlertPanel(alertInfo: alertInfo)
        alertPanel.show()
        self.alertPanel = alertPanel
    }
    
    private func onInWaitingChanged(inWaiting: Bool) {
        if inWaiting {
            routerManager.router(action: .present(.battleCountdown(battleRequestTimeout)))
        } else {
            let topRoute = routerManager.routerState.routeStack.last
            switch topRoute {
            case .battleCountdown(_):
                routerManager.router(action: .dismiss())
            default:
                break
            }
        }
    }

}

extension AnchorView : AnchorPrepareViewDelegate {
    func prepareView(_ view: AnchorPrepareView, didClickBack button: UIButton) {
        routerManager.router(action: .exit)
        isExited = true
    }
    
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton) {
        createRoom()
    }
    
    func prepareViewDidClickSwitchCamera() {
        videoView.startCamera(useFrontCamera: !manager.coreMediaState.isFrontCamera) {} onError: { code, msg in }
    }
}

extension AnchorView: VideoViewDelegate {
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

private extension String {
    static var enterRoomFailedTitleText = localized("live.alert.enterRoom.failed.title")
    static var enterRoomFailedMessageText = localized("live.alert.enterRoom.failed.message.xxx")
    static var confirmText = localized("live.alert.confirm")
    static var roomNameEmptyToast = localized("live.anchor.room.name.empty.toast")
    static var operateFailedText = localized("live.operation.fail.xxx")
    
    static let connectionInviteText = localized("live.connection.invite.desc.xxx")
    static let rejectText = localized("live.alert.refuse")
    static let acceptText = localized("live.anchor.link.accept.title")
    static let battleInvitationText = localized("live.battle.invitation.desc.xxx")
}
