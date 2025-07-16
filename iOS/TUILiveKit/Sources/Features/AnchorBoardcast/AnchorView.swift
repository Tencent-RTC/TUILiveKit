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

public enum AnchorViewFeature {
    case liveData
    case visitorCnt
    case coGuest
    case coHost
    case battle
    
    // Settings
    case soundEffect
}

public class AnchorView: UIView {
    
    public var startLiveBlock:(()->Void)?
    public weak var delegate: AnchorViewDelegate?

    private let liveInfo: LiveInfo
    private var roomId: String {
        liveInfo.roomId
    }
        
    private lazy var manager = AnchorManager(provider: self)
    private lazy var routerManager: AnchorRouterManager = AnchorRouterManager()
    private lazy var routerCenter = AnchorRouterControlCenter(rootViewController: getCurrentViewController() ?? (TUITool.applicationKeywindow().rootViewController ?? UIViewController()), rootRoute: .anchor, routerManager: routerManager, manager: manager, coreView: videoView)
    
    private lazy var coHostRequestPublisher = manager.subscribeCoreViewState(StateSelector(keyPath: \CoHostState.receivedConnectionRequest))
    private lazy var receivedBattleRequestPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorBattleState.receivedBattleRequest))
    private lazy var isInWaitingPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorBattleState.isInWaiting))
    private var cancellableSet = Set<AnyCancellable>()
    
    private let videoView: LiveCoreView
    
    private lazy var livingView: AnchorLivingView = {
        let view = AnchorLivingView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: videoView)
        return view
    }()
    
    private lazy var topGradientView: UIView = {
        var view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()
    
    private lazy var bottomGradientView: UIView = {
        var view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()
    
    private var needPresentAlertInfo: AnchorAlertInfo?
    private lazy var liveStreamObserver = AnchorConnectionObserver(manager: manager)
    private lazy var battleObserver = AnchorBattleObserver(battleManager: manager.battleManager)
    
    public init(liveInfo: LiveInfo, coreView: LiveCoreView, behavior: RoomBehavior = .createRoom) {
        self.liveInfo = liveInfo
        self.videoView = coreView
        super.init(frame: .zero)
        backgroundColor = .black
        self.manager.prepareLiveInfoBeforeEnterRoom(liveInfo: liveInfo)
        self.videoView.videoViewDelegate = self
        self.videoView.registerConnectionObserver(observer: liveStreamObserver)
        self.videoView.registerBattleObserver(observer: battleObserver)
        
        switch behavior {
        case .createRoom:
            startLiveStream()
        case .enterRoom:
            joinSelfCreatedRoom()
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        videoView.stopCamera()
        videoView.stopMicrophone()
        videoView.unregisterConnectionObserver(observer: liveStreamObserver)
        videoView.unregisterBattleObserver(observer: battleObserver)
        LiveKitLog.info("\(#file)", "\(#line)", "deinit AnchorView \(self)")
    }
    
    private var isViewReady: Bool = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
        routerCenter.subscribeRouter()
    }
    
    public override func layoutSubviews() {
        super.layoutSubviews()
        topGradientView.gradient(colors: [.g1.withAlphaComponent(0.3), .clear], isVertical: true)
        bottomGradientView.gradient(colors: [.clear, .g1.withAlphaComponent(0.3)], isVertical: true)
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        livingView.updateRootViewOrientation(isPortrait: isPortrait)
    }
    
    func relayoutCoreView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.leading.trailing.equalToSuperview()
            make.top.equalToSuperview().inset(36.scale375Height())
            make.bottom.equalToSuperview().inset(96.scale375Height())
        })
        sendSubviewToBack(videoView)
    }
}

extension AnchorView {
    public func disableHeaderLiveData(_ isDisable: Bool) {
        disableFeature(.liveData, isDisable: isDisable)
    }
    
    public func disableHeaderVisitorCnt(_ isDisable: Bool) {
        disableFeature(.visitorCnt, isDisable: isDisable)
    }
    
    public func disableFooterCoGuest(_ isDisable: Bool) {
        disableFeature(.coGuest, isDisable: isDisable)
    }
    
    public func disableFooterCoHost(_ isDisable: Bool) {
        disableFeature(.coHost, isDisable: isDisable)
    }
    
    public func disableFooterBattle(_ isDisable: Bool) {
        disableFeature(.battle, isDisable: isDisable)
    }
    
    public func disableFooterSoundEffect(_ isDisable: Bool) {
        disableFeature(.soundEffect, isDisable: isDisable)
    }
    
    func setIcon(_ icon: UIImage, for feature: AnchorViewFeature) {
        // TODO: (gg) need to implementation
    }
    
    private func disableFeature(_ feature: AnchorViewFeature, isDisable: Bool) {
        livingView.disableFeature(feature, isDisable: isDisable)
    }
}

extension AnchorView {
    
    private func constructViewHierarchy() {
        addSubview(videoView)
        addSubview(topGradientView)
        addSubview(bottomGradientView)
        addSubview(livingView)
    }
    
    private func activateConstraints() {
        videoView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalToSuperview().inset(36.scale375Height())
            make.bottom.equalToSuperview().inset(96.scale375Height())
        }
        
        livingView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        topGradientView.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(142.scale375Height())
        }
        
        bottomGradientView.snp.makeConstraints { make in
            make.bottom.leading.trailing.equalToSuperview()
            make.height.equalTo(246.scale375Height())
        }
    }
    
    private func bindInteraction() {
        let mediaState: MediaState = videoView.getState()
        if !mediaState.isCameraOpened {
            videoView.startCamera(useFrontCamera: true) {
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
        if !mediaState.isMicrophoneOpened {
            videoView.startMicrophone() {
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
        subscribeCoHostState()
        subscribeBattleState()
        subscribeSubjects()
    }
    
    private func setupViewStyle() {
        videoView.layer.cornerRadius = 16.scale375()
        videoView.layer.masksToBounds = true
    }
}

// MARK: Action

extension AnchorView {
    func startLiveStream() {
        setLocalVideoMuteImage()
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = roomId
        roomInfo.name = liveInfo.name
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        roomInfo.seatMode = .applyToTake
        roomInfo.maxSeatCount = 9
        
        videoView.startLiveStream(roomInfo: roomInfo) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            manager.onStartLive(isJoinSelf: false, roomInfo: roomInfo)
            startLiveBlock?()
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
        setLocalVideoMuteImage()
        videoView.joinLiveStream(roomId: roomId) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            manager.onStartLive(isJoinSelf: true, roomInfo: roomInfo)
            startLiveBlock?()
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
    
    private func subscribeCoHostState() {
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
                    let alertInfo = AnchorAlertInfo(description: String.localizedReplace(.connectionInviteText, replace: "\(request.userName)"),
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
                        routerManager.router(action: .dismiss(.alert, completion: nil))
                    } defaultClosure: { [weak self] alertPanel in
                        guard let self = self else { return }
                        videoView.respondToCrossRoomConnection(roomId: request.roomId, isAccepted: true) {
                        } onError: { [weak self] err, msg in
                            guard let self = self else { return }
                            let error = InternalError(code: err.rawValue, message: msg)
                            manager.onError(error)
                        }
                        routerManager.router(action: .dismiss(.alert, completion: nil))
                    }
                    if FloatWindow.shared.isShowingFloatWindow() {
                        needPresentAlertInfo = alertInfo
                    } else {
                        routerManager.router(action: .present(.alert(info: alertInfo)))
                    }
                } else {
                    routerManager.router(action: .dismiss(.alert, completion: nil))
                }
            }
            .store(in: &cancellableSet)
    }

    private func setLocalVideoMuteImage() {
        let imageName = TUIGlobalization.getPreferredLanguage() == "en" ? "live_muteImage_en" : "live_muteImage"
        videoView.setLocalVideoMuteImage(
                bigImage:internalImage(imageName) ?? UIImage(),
                smallImage:internalImage("live_muteImage_small") ?? UIImage()
            )
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
    
    private func subscribeSubjects() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                makeToast(message)
            }.store(in: &cancellableSet)
        
        manager.floatWindowSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                delegate?.onClickFloatWindow()
            }
            .store(in: &cancellableSet)

        manager.onEndLivingSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                delegate?.onEndLiving(state: state)
            }
            .store(in: &cancellableSet)
        
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .dropFirst()
            .sink { [weak self] isShow in
                guard let self = self, !isShow, let alertInfo = needPresentAlertInfo else { return }
                routerManager.router(action: .present(.alert(info: alertInfo)))
                needPresentAlertInfo = nil
            }
            .store(in: &cancellableSet)
    }
}

extension AnchorView {
    private func onReceivedBattleRequestChanged(battleUser: AnchorBattleUser?) {
        guard let battleUser = battleUser else {
            routerManager.router(action: .dismiss(.alert, completion: nil))
            return
        }
        let alertInfo = AnchorAlertInfo(description: .localizedReplace(.battleInvitationText, replace: battleUser.userName),
                                        imagePath: battleUser.avatarUrl,
                                        cancelButtonInfo: (String.rejectText, .g3),
                                        defaultButtonInfo: (String.acceptText, .b1)) { [weak self] alertPanel in
            guard let self = self else { return }
            videoView.respondToBattle(battleId: manager.battleState.battleId, isAccepted: false, onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.onResponseBattle()
            }, onError: { _, _ in
                
            })
            routerManager.router(action: .dismiss(.alert, completion: nil))
        } defaultClosure: { [weak self] alertPanel in
            guard let self = self else { return }
            videoView.respondToBattle(battleId: manager.battleState.battleId, isAccepted: true, onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.onResponseBattle()
            }, onError: { _, _ in
                
            })
            routerManager.router(action: .dismiss(.alert, completion: nil))
        }
        if FloatWindow.shared.isShowingFloatWindow() {
            needPresentAlertInfo = alertInfo
        } else {
            routerManager.router(action: .present(.alert(info: alertInfo)))
        }
    }
    
    private func onInWaitingChanged(inWaiting: Bool) {
        if inWaiting {
            routerManager.router(action: .present(.battleCountdown(anchorBattleRequestTimeout)))
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

extension AnchorView: VideoViewDelegate {
    public func createCoGuestView(userInfo: TUIUserInfo) -> UIView? {
        return AnchorCoGuestView(userInfo: userInfo, manager: manager, routerManager: routerManager)
    }
    
    public func updateCoGuestView(coGuestView: UIView, userInfo: TUIUserInfo, modifyFlag: LiveStreamCore.UserInfoModifyFlag) {
        
    }
    
    public func createCoHostView(coHostUser: CoHostUser) -> UIView? {
        return AnchorCoHostView(connectionUser: coHostUser, manager: manager)
    }
    
    public func updateCoHostView(coHostView: UIView, coHostUser: LiveStreamCore.CoHostUser, modifyFlag: LiveStreamCore.UserInfoModifyFlag) {
        
    }
    
    public func createBattleView(battleUser: TUIBattleUser) -> UIView? {
        return AnchorBattleMemberInfoView(manager: manager, userId: battleUser.userId)
    }
    
    public func updateBattleView(battleView: UIView, battleUser: TUIBattleUser) {
        
    }
    
    public func createBattleContainerView() -> UIView? {
        return AnchorBattleInfoView(manager: manager, routerManager: routerManager, coreView: videoView)
    }
    
    public func updateBattleContainerView(battleContainerView: UIView, userInfos: [LiveStreamCore.BattleUserViewModel]) {
        if let battleInfoView = battleContainerView as? AnchorBattleInfoView {
            battleInfoView.updateView(userInfos: userInfos)
        }
    }
}

extension AnchorView: AnchorManagerProvider {
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        videoView.subscribeState(selector)
    }
    
    func getCoreViewState<T>() -> T where T : State {
        videoView.getState()
    }
}

// ** Only should use for test **
extension AnchorView {
    @objc func disableHeaderLiveDataForTest(_ isDisable: NSNumber) {
        disableHeaderLiveData(isDisable.boolValue)
    }
    
    @objc func disableHeaderVisitorCntForTest(_ isDisable: NSNumber) {
        disableHeaderVisitorCnt(isDisable.boolValue)
    }
    
    @objc func disableFooterCoGuestForTest(_ isDisable: NSNumber) {
        disableFooterCoGuest(isDisable.boolValue)
    }
    
    @objc func disableFooterCoHostForTest(_ isDisable: NSNumber) {
        disableFooterCoHost(isDisable.boolValue)
    }
    
    @objc func disableFooterBattleForTest(_ isDisable: NSNumber) {
        disableFooterBattle(isDisable.boolValue)
    }
    
    @objc func disableFooterSoundEffectForTest(_ isDisable: NSNumber) {
        disableFooterSoundEffect(isDisable.boolValue)
    }
}

private extension String {
    static let connectionInviteText = internalLocalized("xxx invite you to host together")
    static let rejectText = internalLocalized("Reject")
    static let acceptText = internalLocalized("Accept")
    static let battleInvitationText = internalLocalized("xxx invite you to battle together")
}
