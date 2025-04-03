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
    
    private lazy var coHostRequestPublisher = manager.subscribeCoreViewState(StateSelector(keyPath: \CoHostState.receivedConnectionRequest))
    private lazy var receivedBattleRequestPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.receivedBattleRequest))
    private lazy var isInWaitingPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.isInWaiting))
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
        manager.onPreviewing()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        prepareView.updateRootViewOrientation(isPortrait: isPortrait)
        livingView.updateRootViewOrientation(isPortrait: isPortrait)
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
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
        }
        videoView.startMicrophone() {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
        }
        subscribeCoHostState()
        subscribeCoGuestState()
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
        videoView.startLiveStream(roomInfo: roomInfo) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            handleAbnormalExitedSence()
            
            manager.onStartLive(isJoinSelf: false, roomInfo: roomInfo)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
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
            manager.onStartLive(isJoinSelf: true, roomInfo: roomInfo)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
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
    
    private func subscribeCoGuestState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] connectedUserList in
                guard let self = self else { return }
                manager.onCoGuestConnectUserChanged(connectUserList: connectedUserList)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeCoHostState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \CoHostState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] connectedUserList in
                guard let self = self else { return }
                manager.onCoHostConnectUserChanged(connectUserList: connectedUserList)
            }
            .store(in: &cancellableSet)
        
        coHostRequestPublisher.receive(on: RunLoop.main)
            .sink { [weak self] connectionRequest in
                guard let self = self else { return }
                if !manager.coreCoGuestState.applicantList.isEmpty {
                    // If received linkmic request first, reject connection auto.
                    if let request = connectionRequest {
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: false) {
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
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: false) {
                        } onError: { [weak self] err, msg in
                            guard let self = self else { return }
                            let error = InternalError(code: err.rawValue, message: msg)
                            manager.onError(error)
                        }
                        alertPanel.dismiss()
                    } defaultClosure: { [weak self] alertPanel in
                        guard let self = self else { return }
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: true) {
                        } onError: { [weak self] err, msg in
                            guard let self = self else { return }
                            let error = InternalError(code: err.rawValue, message: msg)
                            manager.onError(error)
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
                manager.onResponseBattle()
            }, onError: { _, _ in
                
            })
            
        } defaultClosure: { [weak self] alertPanel in
            alertPanel.dismiss()
            guard let self = self else { return }
            videoView.respondToBattle(battleId: manager.battleState.battleId, isAccepted: true, onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.onResponseBattle()
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
        videoView.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
    }
}

extension AnchorView: VideoViewDelegate {
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

private extension String {
    static let connectionInviteText = localized("xxx invite you to host together")
    static let rejectText = localized("Reject")
    static let acceptText = localized("Accept")
    static let battleInvitationText = localized("xxx invite you to battle together")
}
