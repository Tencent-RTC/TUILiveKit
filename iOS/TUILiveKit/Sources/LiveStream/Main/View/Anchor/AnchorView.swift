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
    private let roomId: String
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    
    private lazy var coHostRequestPublisher = manager.coHostManager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.receivedConnectionRequest))
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
    
    private lazy var battleInfoView: BattleInfoView = {
        let view = BattleInfoView(manager: manager, routerManager: routerManager, isOwner: true)
        return view
    }()
    
    lazy var beautyPanelView: UIView = {
        let view = BeautyView(roomId: roomId)
        view.backClosure =  { [weak self] in
            guard let self = self else { return }
            routerManager.router(action: .dismiss())
        }
        view.frame = CGRect(x: -100, y: -100, width: 0, height: 0)
        return view
    }()
    
    private weak var alertPanel: LSAlertPanel?

    init(roomId: String, manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.videoView = coreView
        super.init(frame: .zero)
        self.videoView.videoViewDelegate = self
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        let liveStatus = manager.roomState.liveStatus
        if liveStatus == .previewing {
            videoView.stopCamera()
            videoView.stopMicrophone()
        }
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
}



extension AnchorView {
    
    private func constructViewHierarchy() {
        addSubview(videoView)
        addSubview(battleInfoView)
        addSubview(prepareView)
        addSubview(livingView)
        addSubview(beautyPanelView)
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
        
        battleInfoView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
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
            manager.update(liveStatus: .pushing)
            manager.updateRoomState(roomInfo: roomInfo)
            manager.syncLiveInfoToService()
            manager.fetchAudienceList()
            manager.updateOwnerUserInfo()
            manager.fetchSeatList()
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

    private func subscribeCoHostState() {
        coHostRequestPublisher.receive(on: RunLoop.main)
            .sink { [weak self] connectionRequest in
                guard let self = self else { return }
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
}

extension AnchorView : AnchorPrepareViewDelegate {
    
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton) {
        createRoom()
    }
    
}

extension AnchorView: VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView? {
        return CoGuestView(userInfo: userInfo, manager: manager)
    }
    
    func updateCoGuestView(userInfo: TUIUserInfo, coGuestView: UIView) {
        
    }
    
    func createCoHostView(connectionUser: TUIConnectionUser) -> UIView? {
        return CoHostView(connectionUser: connectionUser, manager: manager)
    }
    
    func updateCoHostView(connectionUser: TUIConnectionUser, coHostView: UIView) {
        
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
