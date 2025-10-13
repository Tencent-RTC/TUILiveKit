//
//  AudienceView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/19.
//

import Foundation
import RTCCommon
import Combine
import TUICore
import AtomicXCore
import RTCRoomEngine

public protocol RotateScreenDelegate: AnyObject {
    func rotateScreen(isPortrait: Bool)
}

class AudienceView: RTCBaseView {
    let roomId: String
    
    weak var rotateScreenDelegate: RotateScreenDelegate?
    
    // MARK: - private property
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private var cancellableSet: Set<AnyCancellable> = []
    
    // MARK: - property: view
    private let videoView: LiveCoreView
    private lazy var liveStreamObserver = AudienceConnectionObserver(manager: manager)
    private lazy var battleObserver = AudienceBattleManagerObserver(battleManager: manager.battleManager)
    
    private var panDirection: PanDirection = .none
    enum PanDirection {
        case left, right, none
    }
    
    lazy var livingView: AudienceLivingView = {
        let view = AudienceLivingView(manager: manager, routerManager: routerManager, coreView: videoView)
        view.rotateScreenDelegate = self
        return view
    }()
    
    lazy var leaveButton: UIButton = {
        let button = UIButton()
        button.setImage(internalImage("live_leave_icon"), for: .normal)
        button.imageEdgeInsets = UIEdgeInsets(top: 2.scale375(), left: 2.scale375(), bottom: 2.scale375(), right: 2.scale375())
        return button
    }()
    
    lazy var restoreClearButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .black.withAlphaComponent(0.2)
        button.setImage(internalImage("live_restore_clean_icon"), for: .normal)
        button.layer.cornerRadius = 20.scale375()
        button.isHidden = true
        return button
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
    
    lazy var topGradientView: UIView = {
        var view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()
    
    lazy var bottomGradientView: UIView = {
        var view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()
    
    init(roomId: String, manager: AudienceManager, routerManager: AudienceRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.videoView = coreView
        super.init(frame: .zero)
        videoView.setLiveID(roomId)
        self.videoView.videoViewDelegate = self
        self.videoView.registerConnectionObserver(observer: liveStreamObserver)
        self.videoView.registerBattleObserver(observer: battleObserver)
        self.manager.prepareRoomIdBeforeEnterRoom(roomId: roomId)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
        videoView.unregisterConnectionObserver(observer: liveStreamObserver)
        videoView.unregisterBattleObserver(observer: battleObserver)
        LiveKitLog.info("\(#file)", "\(#line)", "deinit AudienceView \(self)")
    }
    
    override func constructViewHierarchy() {
        addSubview(coverBgView)
        addSubview(videoView)
        addSubview(topGradientView)
        addSubview(bottomGradientView)
        addSubview(livingView)
        addSubview(leaveButton)
        addSubview(restoreClearButton)
    }
    
    override func activateConstraints() {
        coverBgView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        videoView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        livingView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        if WindowUtils.isPortrait {
            leaveButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().inset(20.scale375Width())
                make.top.equalToSuperview().offset(70.scale375Height())
                make.width.equalTo(24.scale375Width())
                make.height.equalTo(24.scale375Width())
            }
            restoreClearButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().inset(16.scale375())
                make.bottom.equalToSuperview().inset(40.scale375Height())
                make.width.height.equalTo(40.scale375())
            }
            topGradientView.snp.remakeConstraints { make in
                make.top.leading.trailing.equalToSuperview()
                make.height.equalTo(142.scale375Height())
            }
            bottomGradientView.snp.remakeConstraints { make in
                make.bottom.leading.trailing.equalToSuperview()
                make.height.equalTo(246.scale375Height())
            }
        } else {
            leaveButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().inset(20.scale375())
                make.top.equalToSuperview().offset(20.scale375())
                make.width.equalTo(24.scale375())
                make.height.equalTo(24.scale375())
            }
            restoreClearButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().inset(16.scale375())
                make.bottom.equalToSuperview().inset(40.scale375())
                make.width.height.equalTo(40.scale375())
            }
            topGradientView.snp.remakeConstraints { make in
                make.top.leading.trailing.equalToSuperview()
                make.height.equalTo(142.scale375())
            }
            bottomGradientView.snp.remakeConstraints { make in
                make.bottom.leading.trailing.equalToSuperview()
                make.height.equalTo(246.scale375())
            }
        }
    }
    
    override func bindInteraction() {
        subscribeOrientationChange()
        subscribeRoomState()
        subscribeMediaState()
        subscribeSubject()
        setupSlideToClear()
        leaveButton.addTarget(self, action: #selector(leaveButtonClick), for: .touchUpInside)
        restoreClearButton.addTarget(self, action: #selector(restoreLivingView), for: .touchUpInside)
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        topGradientView.gradient(colors: [.g1.withAlphaComponent(0.3), .clear], isVertical: true)
        bottomGradientView.gradient(colors: [.clear, .g1.withAlphaComponent(0.3)], isVertical: true)
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
    private func subscribeOrientationChange() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(handleOrientationChange),
            name: Notification.Name.TUILiveKitRotateScreenNotification,
            object: nil
        )
    }

    private func subscribeRoomState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceRoomState.liveStatus))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .finished:
                        routeToAudienceView()
                    default: break
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \AudienceRoomState.backgroundUrl))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] url in
                guard let self = self else { return }
                coverBgView.kf.setImage(with: URL(string: url), placeholder: internalImage("live_edit_info_default_cover_image"))
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeMediaState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceMediaState.isAudioLocked))
            .removeDuplicates()
            .dropFirst()
            .receive(on: RunLoop.main)
            .sink { [weak self] isAudioLocked in
                guard let self = self, manager.coGuestState.coGuestStatus == .linking else { return }
                manager.toastSubject.send(isAudioLocked ? .mutedAudioText : .unmutedAudioText)
            }
            .store(in: &cancellableSet)
        
        
        manager.subscribeState(StateSelector(keyPath: \AudienceMediaState.isVideoLocked))
            .removeDuplicates()
            .dropFirst()
            .receive(on: RunLoop.main)
            .sink { [weak self] isVideoLocked in
                guard let self = self, manager.coGuestState.coGuestStatus == .linking else { return }
                manager.toastSubject.send(isVideoLocked ? .mutedVideoText : .unmutedVideoText)
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
        
    private func onKickedByAdmin() {
        manager.toastSubject.send(.kickedOutText)
        isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
            guard let self = self else { return }
            isUserInteractionEnabled = true
            routerManager.router(action: .exit)
        }
    }
    
    @objc func handleOrientationChange() {
        activateConstraints()
    }
    
    @objc func leaveButtonClick() {
        rotateScreenDelegate?.rotateScreen(isPortrait: true)

        let selfUserId = manager.coreUserState.selfInfo.userId
        if !manager.coreCoGuestState.seatList.contains(where: { $0.userId == selfUserId }) {
            leaveRoom()
            return
        }
        var items: [ActionItem] = []
        let lineConfig = ActionItemDesignConfig(lineWidth: 1, titleColor: .redColor)
        lineConfig.backgroundColor = .white
        lineConfig.lineColor = .g8
        
        let title: String = .endLiveOnLinkMicText
        let endLinkMicItem = ActionItem(title: .endLiveLinkMicDisconnectText, designConfig: lineConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            videoView.terminateIntraRoomConnection()
            routerManager.router(action: .dismiss())
        })
        items.append(endLinkMicItem)
        
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endLiveItem = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            routerManager.router(action: .dismiss())
            leaveRoom()
        })
        items.append(endLiveItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(title: title, items: items, cancelText: .cancelText))))
    }
    
    func leaveRoom() {
        videoView.leaveLiveStream() { [weak self] in
            guard let self = self else { return }
            manager.onLeaveLive()
        } onError: { _, _ in
        }
        routerManager.router(action: .exit)
        TUICore.notifyEvent(TUICore_PrivacyService_ROOM_STATE_EVENT_CHANGED,
                            subKey: TUICore_PrivacyService_ROOM_STATE_EVENT_SUB_KEY_END,
                            object: nil,
                            param: nil)
    }
}

// MARK: - Slide to clear
extension AudienceView {
    private func setupSlideToClear() {
        let panGesture = UIPanGestureRecognizer(target: self, action: #selector(handlePan(_:)))
        panGesture.delegate = self
        addGestureRecognizer(panGesture)
    }
    
    @objc private func handlePan(_ gesture: UIPanGestureRecognizer) {
        let translation = gesture.translation(in: self)
        let velocity = gesture.velocity(in: self)
        
        switch gesture.state {
        case .began:
            panDirection = velocity.x > 0 ? .right : .left
        case .changed:
            guard isValidPan() else { return }
            if panDirection == .left {
                livingView.transform = CGAffineTransform(translationX: bounds.width + translation.x, y: 0)
            } else if translation.x > 0 {
                livingView.transform = CGAffineTransform(translationX: translation.x, y: 0)
            }
        case .ended, .cancelled:
            guard isValidPan() else { return }
            let isSameDirection = velocity.x > 0 && panDirection == .right || velocity.x < 0 && panDirection == .left
            let shouldComplete = isSameDirection && (abs(translation.x) > 100 || abs(velocity.x) > 800)
            if shouldComplete {
                panDirection == .right ? hideLivingView() : restoreLivingView()
            } else {
                resetLivingView()
            }
            panDirection = .none
        default: break
        }
    }
    
    private func isValidPan() -> Bool {
        return (panDirection == .right && !isLivingViewMoved()) || (panDirection == .left && isLivingViewMoved())
    }
    
    private func hideLivingView() {
        UIView.animate(withDuration: 0.3, animations: { [weak self] in
            guard let self = self else { return }
            livingView.transform = CGAffineTransform(translationX: UIScreen.main.bounds.width, y: 0)
        })
        restoreClearButton.isHidden = false
        livingView.setGiftPureMode(true)
    }
    
    @objc private func restoreLivingView() {
        UIView.animate(withDuration: 0.3) {
            self.livingView.transform = CGAffineTransform(translationX: 0, y: 0)
        }
        restoreClearButton.isHidden = true
        livingView.setGiftPureMode(false)
    }
        
    private func resetLivingView() {
        UIView.animate(withDuration: 0.3) { [weak self] in
            guard let self = self else { return }
            if panDirection == .right {
                livingView.transform = .identity
            } else {
                livingView.transform = CGAffineTransform(translationX: bounds.width, y: 0)
            }
        }
    }
    
    private func isLivingViewMoved() -> Bool {
        !restoreClearButton.isHidden
    }
}

extension AudienceView {
    func joinLiveStream(onComplete: @escaping (Result<Void, InternalError>) -> Void) {
        let imageName = TUIGlobalization.getPreferredLanguage() == "en" ? "live_muteImage_en" : "live_muteImage"
        videoView
            .setLocalVideoMuteImage(
                bigImage:internalImage(imageName) ?? UIImage(),
                smallImage:internalImage("live_muteImage_small") ?? UIImage()
            )
        videoView.joinLiveStream(roomId: roomId) { [weak self] liveInfo in
            guard let self = self else { return }
            manager.onJoinLive(liveInfo: liveInfo)
            livingView.initComponentView()
            livingView.isHidden = false
            onComplete(.success(()))
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
            onComplete(.failure(error))
        }
    }
}

extension AudienceView: VideoViewDelegate {
    func createCoGuestView(seatInfo: TUISeatFullInfo, viewLayer: ViewLayer) -> UIView? {
        switch viewLayer {
        case .foreground:
            if let userId = seatInfo.userId, !userId.isEmpty {
                let userInfo = seatFullInfoToUserInfo(seatInfo: seatInfo)
                return AudienceCoGuestView(userInfo: userInfo, manager: manager, routerManager: routerManager)
            }
            return AudienceEmptySeatView(seatInfo: seatInfo, manager: manager, routerManager: routerManager, coreView: videoView)
        case .background:
            if let userId = seatInfo.userId, !userId.isEmpty {
                return AudienceBackgroundWidgetView(avatarUrl: seatInfo.userAvatar ?? "")
            }
            return nil
        }
    }
    
    func createCoHostView(seatInfo: TUISeatFullInfo, viewLayer: ViewLayer) -> UIView? {
        switch viewLayer {
        case .foreground:
            if let userId = seatInfo.userId, !userId.isEmpty {
                let coHostUser = seatFullInfoToCoHostUser(seatInfo: seatInfo)
                return AudienceCoHostView(connectionUser: coHostUser, manager: manager)
            }
            return AudienceEmptySeatView(seatInfo: seatInfo, manager: manager, routerManager: routerManager, coreView: videoView)
        case .background:
            if let userId = seatInfo.userId, !userId.isEmpty {
                return AudienceBackgroundWidgetView(avatarUrl: seatInfo.userAvatar ?? "")
            }
            return nil
        }
    }
    
    func createBattleView(battleUser: TUIBattleUser) -> UIView? {
        return AudienceBattleMemberInfoView(manager: manager, userId: battleUser.userId)
    }
    
    func createBattleContainerView() -> UIView? {
        return AudienceBattleInfoView(manager: manager, routerManager: routerManager, isOwner: true, coreView: videoView)
    }
    
    func updateBattleContainerView(battleContainerView: UIView, userInfos: [AtomicXCore.BattleUserViewModel]) {
        if let battleInfoView = battleContainerView as? AudienceBattleInfoView {
            battleInfoView.updateView(userInfos: userInfos)
        }
    }
    
    private func seatFullInfoToUserInfo(seatInfo: TUISeatFullInfo) -> TUIUserInfo {
        let userInfo = TUIUserInfo()
        userInfo.userId = seatInfo.userId ?? ""
        userInfo.userName = seatInfo.userName ?? ""
        userInfo.avatarUrl = seatInfo.userAvatar ?? ""
        userInfo.userRole = .generalUser
        userInfo.hasVideoStream = seatInfo.userCameraStatus == .opened
        userInfo.hasAudioStream = seatInfo.userMicrophoneStatus == .opened
        return userInfo
    }
    
    private func seatFullInfoToCoHostUser(seatInfo: TUISeatFullInfo) -> CoHostUser {
        let user = TUIConnectionUser()
        user.userId = seatInfo.userId ?? ""
        user.userName = seatInfo.userName ?? ""
        user.avatarUrl = seatInfo.userAvatar ?? ""
        user.roomId = seatInfo.roomId
        
        let coHostUser = CoHostUser()
        coHostUser.connectionUser = user
        coHostUser.hasVideoStream = seatInfo.userCameraStatus == .opened
        coHostUser.hasAudioStream = seatInfo.userMicrophoneStatus == .opened
        return coHostUser
    }
}

extension AudienceView: UIGestureRecognizerDelegate {
    override func gestureRecognizerShouldBegin(_ gesture: UIGestureRecognizer) -> Bool {
        guard let pan = gesture as? UIPanGestureRecognizer else { return true }
        let velocity = pan.velocity(in: self)
        return abs(velocity.x) > abs(velocity.y)
    }
}

extension AudienceView: RotateScreenDelegate {
    func rotateScreen(isPortrait: Bool) {
        rotateScreenDelegate?.rotateScreen(isPortrait: isPortrait)
    }
}

fileprivate extension String {
    static let kickedOutText = internalLocalized("You have been kicked out of the room")
    static let mutedAudioText = internalLocalized("The anchor has muted you")
    static let unmutedAudioText = internalLocalized("The anchor has unmuted you")
    static let mutedVideoText = internalLocalized("The anchor disabled your video")
    static let unmutedVideoText = internalLocalized("The anchor enabled your video")
    static let endLiveOnLinkMicText = internalLocalized("You are currently co-guesting with other streamers. Would you like to [End Co-guest] or [Exit Live] ?")
    static let endLiveLinkMicDisconnectText = internalLocalized("End Co-guest")
    static let confirmCloseText = internalLocalized("Exit Live")
    static let cancelText = internalLocalized("Cancel")
}
