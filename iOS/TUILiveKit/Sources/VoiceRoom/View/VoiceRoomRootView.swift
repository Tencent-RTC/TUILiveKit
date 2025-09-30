//
//  VoiceRoomRootView.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import Combine
import Kingfisher
import SnapKit
import TUICore
import RTCCommon
import RTCRoomEngine
import AtomicXCore
import AtomicX

protocol VoiceRoomRootViewDelegate: AnyObject {
    func rootView(_ view: VoiceRoomRootView, showEndView endInfo: [String:Any], isAnchor: Bool)
}

class VoiceRoomRootView: RTCBaseView {
    weak var delegate: VoiceRoomRootViewDelegate?
    
    private let manager: VoiceRoomManager
    private let seatGridView: SeatGridView
    private let routerManager: VRRouterManager
    private let kTimeoutValue: TimeInterval = 60
    private let isOwner: Bool
    private let giftCacheService = GiftManager.shared.giftCacheService
    private var cancellableSet = Set<AnyCancellable>()
    private var isExited: Bool = false
    private let defaultTemplateId: UInt = 70
    
    private let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()

    private lazy var karaokeManager: KaraokeManager = {
        let manager = KaraokeManager(roomId: self.manager.roomState.roomId)
        return manager
    }()

    private var ktvView: KtvView?

    private let backgroundGradientView: UIView = {
        var view = UIView()
        return view
    }()

    private lazy var topView: VRTopView = {
        let view = VRTopView(manager: manager, routerManager: routerManager)
        return view
    }()
    
    private lazy var bottomMenu : VRBottomMenuView = {
        let view = VRBottomMenuView(manager: manager, routerManager: routerManager, coreView: seatGridView, isOwner: isOwner)
        view.songListButtonAction = { [weak self] in
            guard let self = self , let vc = WindowUtils.getCurrentWindowViewController() else { return }
            let isKTV = manager.roomState.layoutType == .KTVRoom
            let songListView = SongListViewController(karaokeManager: self.karaokeManager,isOwner: isOwner,isKTV: isKTV)
            vc.present(songListView, animated: true)
        }
        return view
    }()

    private let muteMicrophoneButton: UIButton = {
        let button = UIButton(frame: .zero)
        button.setImage(internalImage("live_open_mic_icon"), for: .normal)
        button.setImage(internalImage("live_close_mic_icon"), for: .selected)
        button.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        button.layer.borderWidth = 1
        button.layer.cornerRadius = 16.scale375Height()
        return button
    }()
    
    private lazy var barrageButton: BarrageInputView = {
        let view = BarrageInputView(roomId: manager.roomState.roomId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 1
        view.layer.cornerRadius = 18.scale375Height()
        view.backgroundColor = .clear
        return view
    }()
    
    private lazy var barrageDisplayView: BarrageStreamView = {
        let view = BarrageStreamView(liveId: manager.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    private lazy var giftDisplayView: GiftPlayView = {
        let view = GiftPlayView(roomId: manager.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    init(frame: CGRect,
         roomId: String,
         seatGridView: SeatGridView,
         manager: VoiceRoomManager,
         routerManager: VRRouterManager,
         isCreate: Bool) {
        self.manager = manager
        self.routerManager = routerManager
        self.isOwner = isCreate
        self.seatGridView = seatGridView
        super.init(frame: frame)
        if isCreate {
            start(roomId: roomId)
        } else {
            join(roomId: roomId)
        }
        seatGridView.addObserver(observer: self)
    }
    
    deinit {
        seatGridView.removeObserver(observer: self)
        TUICore.notifyEvent(TUICore_PrivacyService_ROOM_STATE_EVENT_CHANGED,
                            subKey: TUICore_PrivacyService_ROOM_STATE_EVENT_SUB_KEY_END,
                            object: nil,
                            param: nil)
        print("deinit \(type(of: self))")
    }
    
    override func constructViewHierarchy() {
        addSubview(backgroundImageView)
        addSubview(backgroundGradientView)
        addSubview(barrageDisplayView)
        addSubview(seatGridView)
        addSubview(giftDisplayView)
        addSubview(topView)
        addSubview(bottomMenu)
        addSubview(barrageButton)
        addSubview(muteMicrophoneButton)
    }
    
    override func activateConstraints() {
        backgroundImageView.snp.makeConstraints { (make) in
            make.edges.equalToSuperview()
        }
        backgroundGradientView.snp.makeConstraints { (make) in
            make.edges.equalToSuperview()
        }
        topView.snp.makeConstraints { make in
            make.left.right.equalToSuperview()
            make.top.equalToSuperview().offset(54.scale375Height())
        }

        seatGridView.snp.makeConstraints { make in
            make.top.equalTo(topView.snp.bottom).offset(40.scale375())
            make.height.equalTo(230.scale375())
            make.left.right.equalToSuperview()
        }

        bottomMenu.snp.makeConstraints { make in
            make.bottom.equalToSuperview().offset(-34.scale375Height())
            make.trailing.equalToSuperview()
            make.height.equalTo(36)
        }
        barrageDisplayView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.bottom.equalTo(barrageButton.snp.top).offset(-20)
            make.width.equalTo(305.scale375())
            make.height.equalTo(212.scale375Height())
        }
        barrageButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375())
            make.centerY.equalTo(bottomMenu.snp.centerY)
            make.width.equalTo(130.scale375())
            make.height.equalTo(36.scale375Height())
        }
        muteMicrophoneButton.snp.makeConstraints { make in
            make.leading.equalTo(barrageButton.snp.trailing).offset(8.scale375())
            make.centerY.equalTo(barrageButton.snp.centerY)
            make.size.equalTo(CGSize(width: 32.scale375Height(), height: 32.scale375Height()))
        }
        giftDisplayView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        // Top view interaction.
        topView.delegate = self
        subscribeRoomState()
        subscribeUserState()
        muteMicrophoneButton.addTarget(self, action: #selector(muteMicrophoneButtonClick(sender:)), for: .touchUpInside)
        manager.refreshSelfInfo()
    }
}

extension VoiceRoomRootView {
    @objc
    func muteMicrophoneButtonClick(sender: UIButton) {
        muteMicrophone(mute: !sender.isSelected)
    }
    
    func muteMicrophone(mute: Bool) {
        if mute {
            seatGridView.muteMicrophone()
        } else {
            seatGridView.unmuteMicrophone(onSuccess: {
            }, onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(code: code, message: message)
                manager.onError(error.localizedMessage)
            })
        }
    }
    
    func startMicrophone() {
        seatGridView.startMicrophone {
            
        } onError: { [weak self] code, message in
            if code == TUIError.openMicrophoneNeedSeatUnlock.rawValue {
                // Seat muted will pops up in unmuteMicrophone, so no processing is needed here
                return
            }
            guard let self = self else { return }
            let error = InternalError(code: code, message: message)
            manager.onError(error.localizedMessage)
        }
    }
    
    func stopMicrophone() {
        seatGridView.stopMicrophone()
    }
}

extension VoiceRoomRootView {
    private func start(roomId: String) {
        let liveInfo = TUILiveInfo()
        let roomState = manager.roomState
        liveInfo.roomId = roomId
        liveInfo.name = roomState.roomName
        liveInfo.seatMode = manager.roomParams.seatMode
        liveInfo.coverUrl = manager.roomState.coverURL
        liveInfo.backgroundUrl = manager.roomState.backgroundURL
        liveInfo.isPublicVisible = manager.roomState.liveExtraInfo.liveMode == .public
        liveInfo.maxSeatCount = manager.roomParams.maxSeatCount
        liveInfo.isSeatEnabled = true
        liveInfo.keepOwnerOnSeat = true
        liveInfo.seatLayoutTemplateId = defaultTemplateId

        seatGridView.startVoiceRoom(liveInfo: liveInfo) { [weak self] liveInfo in
            guard let self = self else { return }
            handleAbnormalExitedSence()
            manager.onStartVoiceRoom(liveInfo: liveInfo)
            didEnterRoom()
        } onError: {  [weak self] code, message in
            guard let self = self else { return }
            manager.onError(.enterRoomFailedText)
        }
    }
    
    private func join(roomId: String) {
        
        seatGridView.joinVoiceRoom(roomId: roomId) { [weak self] liveInfo in
            guard let self = self else { return }
            manager.onJoinVoiceRoom(liveInfo: liveInfo)
            didEnterRoom()
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code, message: message)
            manager.onError(error.localizedMessage)
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .exit)
            }
        }
    }
    
    func didEnterRoom() {
        if isOwner {
            TUICore.notifyEvent(TUICore_PrivacyService_ROOM_STATE_EVENT_CHANGED,
                                subKey: TUICore_PrivacyService_ROOM_STATE_EVENT_SUB_KEY_START,
                                object: nil,
                                param: nil)
        }
        initComponentView()
        karaokeManager.synchronizeMetadata(isOwner: isOwner)
        handleRoomLayoutType()
    }

    func handleRoomLayoutType() {
        manager.onSetHasKTVAbility(hasKTVAbility: true)
        func setupKTVView(isOwner: Bool, isKTV: Bool) {
            ktvView = KtvView(
                karaokeManager: karaokeManager,
                isOwner: isOwner,
                isKTV: isKTV
            )

            guard let ktvView = ktvView else { return }
            addSubview(ktvView)

            if isKTV {
                seatGridView.snp.remakeConstraints { make in
                    make.top.equalTo(ktvView.snp.bottom).offset(20.scale375())
                    make.height.equalTo(230.scale375())
                    make.left.right.equalToSuperview()
                }

                ktvView.snp.remakeConstraints { make in
                    make.top.equalTo(topView.snp.bottom).offset(20.scale375())
                    make.height.equalTo(168.scale375())
                    make.left.equalToSuperview().offset(16.scale375())
                    make.right.equalToSuperview().offset(-16.scale375())
                }
            } else {
                seatGridView.snp.remakeConstraints { make in
                    make.top.equalTo(topView.snp.bottom).offset(40.scale375())
                    make.height.equalTo(230.scale375())
                    make.left.right.equalToSuperview()
                }

                ktvView.snp.remakeConstraints { make in
                    make.top.equalTo(seatGridView.snp.bottom).offset(20.scale375())
                    make.trailing.equalToSuperview().inset(20.scale375())
                    make.width.equalTo(160.scale375())
                    make.height.equalTo(137.scale375())
                }
            }
        }

        if isOwner {
            let isKTV = manager.roomState.layoutType == .KTVRoom
            let layoutType = isKTV ? "KTVRoom" : "ChatRoom"
            let metadata = ["LayoutType": layoutType]

            TUIRoomEngine.sharedInstance().setRoomMetadataByAdmin(metadata, onSuccess: { [weak self] in
                setupKTVView(isOwner: true, isKTV: isKTV)
            }, onError: { error, message in
            })
        }
        else {
            TUIRoomEngine.sharedInstance().getRoomMetadata(["LayoutType"], onSuccess: { [weak self] response in
                guard let layoutType = response["LayoutType"] else {
                    return
                }
                setupKTVView(isOwner: false, isKTV: layoutType == "KTVRoom")
            }, onError: { error, message in
            })
        }
    }


    func onExit() {
        isExited = true
    }
    
    private func handleAbnormalExitedSence() {
        if isExited {
            seatGridView.stopVoiceRoom {_ in 
            } onError: { code, message in
            }
        }
    }
    
    func initComponentView() {
        initTopView()
    }
    
    func initTopView() {
        topView.initialize(roomId: manager.roomState.roomId)
    }
}

// MARK: - Audience Route
extension VoiceRoomRootView {
    private func routeToAudienceView() {
        routerManager.router(action: .routeTo(.audience))
    }
    
    private func routeToAnchorView() {
        routerManager.router(action: .routeTo(.anchor))
    }
}

// MARK: - EndView

extension VoiceRoomRootView {
    
    func stopVoiceRoom() {
        fetchRoomStatistics { [weak self] in
            guard let self = self else { return }
            self.seatGridView.stopVoiceRoom { [weak self] _ in
                guard let self = self else { return }
                manager.onStopVoiceRoom()
                karaokeManager.exit()
                showAnchorEndView()
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(code: code, message: message)
                manager.onError(error.localizedMessage)
            }
        }
    }
    
    private func fetchRoomStatistics(completion: @escaping () -> Void) {
        let roomId = manager.roomState.roomId
        let group = DispatchGroup()
        
        group.enter()
        manager.fetchGiftCount(roomId: roomId) {
            group.leave()
        } onError: { _ in
            group.leave()
        }
        
        group.enter()
        manager.fetchLikeCount(roomId: roomId) {
            group.leave()
        } onError: { _ in
            group.leave()
        }
        
        group.enter()
        manager.fetchViewCount(roomId: roomId) {
            group.leave()
        } onError: { _ in
            group.leave()
        }
        
        group.notify(queue: .main, execute: completion)
    }
    
    private func showAnchorEndView() {
        let liveDataModel = AnchorEndStatisticsViewInfo(roomId: manager.roomState.roomId,
                                                        liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(manager.roomState.createTime / 1_000))),
                                                        viewCount: manager.roomState.liveExtraInfo.maxAudienceCount,
                                                        messageCount: barrageDisplayView.getBarrageCount(),
                                                        giftTotalCoins: manager.roomState.liveExtraInfo.giftTotalCoins,
                                                        giftTotalUniqueSender: manager.roomState.liveExtraInfo.giftTotalUniqueSender,
                                                        likeTotalUniqueSender: manager.roomState.liveExtraInfo.likeTotalUniqueSender)
        delegate?.rootView(self, showEndView: ["data": liveDataModel], isAnchor: true)
    }
    
    private func showAudienceEndView() {
        if !isOwner {
            let info: [String: Any] = [
                "roomId": manager.roomState.roomId,
                "avatarUrl": manager.coreLiveState.liveOwner.avatarURL,
                "userName": manager.userState.selfInfo.userName
            ]
            delegate?.rootView(self, showEndView: info, isAnchor: false)
        }
    }
}

// MARK: - Private

extension VoiceRoomRootView {
    private func subscribeRoomState() {
        subscribeRoomBackgroundState()
        subscribeRoomOwnerState()
    }
    
    private func subscribeUserState() {
        subscribeUserIsOnSeatState()
        subscribeUserLinkState()
    }
}

// MARK: - SubscribeRoomState

extension VoiceRoomRootView {
    private func subscribeRoomBackgroundState() {
        manager.subscribeState(StateSelector(keyPath: \VRRoomState.backgroundURL))
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: URL(string: url), placeholder: UIImage.placeholderImage)
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomOwnerState() {
        manager.subscribeCoreState(StatePublisherSelector(keyPath: \LiveListState.currentLive.liveOwner.userId))
            .compactMap { $0 }
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerId in
                guard let self = self else { return }
                self.barrageDisplayView.setOwnerId(ownerId)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - SubscribeUserState

extension VoiceRoomRootView {
    private func subscribeUserIsOnSeatState() {
        let selfInfoPublisher = manager.subscribeState(StateSelector(keyPath: \VRUserState.selfInfo))
        let seatListPublisher = manager.subscribeCoreState(StatePublisherSelector(keyPath: \LiveSeatState.seatList))
        seatListPublisher
            .combineLatest(selfInfoPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList, selfInfo in
                guard let self = self else { return }
                guard let seatInfo = seatList.first(where: { $0.userInfo.userId == selfInfo.userId }) else {
                    muteMicrophoneButton.isHidden = true
                    return
                }
                muteMicrophoneButton.isHidden = false
                muteMicrophoneButton.isSelected = seatInfo.userInfo.microphoneStatus == .off
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeUserLinkState() {
        manager.subscribeState(StateSelector(keyPath: \VRUserState.linkStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                case .linking:
                    muteMicrophone(mute: false)
                    startMicrophone()
                case .none:
                    stopMicrophone()
                default: break
                }
            }.store(in: &cancellableSet)
    }
}

// MARK: - TopViewDelegate

extension VoiceRoomRootView: VRTopViewDelegate {
    func topView(_ topView: VRTopView, tap event: VRTopView.TapEvent, sender: Any?) {
        switch event {
        case .stop:
            if isOwner {
                anchorStopButtonClick()
            } else {
                audienceLeaveButtonClick()
            }
        case .roomInfo:
            routerManager.router(action: .present(.roomInfo))
        case .audienceList:
            routerManager.router(action: .present(.recentViewer))
        }
    }
    
    private func anchorStopButtonClick() {
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let item = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.stopVoiceRoom()
            self.routerManager.router(action: .dismiss())
        })
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: [item], cancelText: .cancelText))))
    }
    
    private func audienceLeaveButtonClick() {
        let selfUserId = manager.userState.selfInfo.userId
        if !manager.coreSeatState.seatList.contains(where: { $0.userInfo.userId == selfUserId }) {
            leaveRoom()
            routerManager.router(action: .exit)
            return
        }
        var items: [ActionItem] = []
        let lineConfig = ActionItemDesignConfig(lineWidth: 1, titleColor: .redColor)
        lineConfig.backgroundColor = .white
        lineConfig.lineColor = .g8
        
        let title: String = .exitLiveOnLinkMicText
        let endLinkMicItem = ActionItem(title: .exitLiveLinkMicDisconnectText, designConfig: lineConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            seatGridView.leaveSeat {
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(code: code, message: message)
                manager.onError(error.localizedMessage)
            }

            routerManager.router(action: .dismiss())
        })
        items.append(endLinkMicItem)
        
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endLiveItem = ActionItem(title: .exitLiveText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            leaveRoom()
            routerManager.router(action: .dismiss())
            routerManager.router(action: .exit)
        })
        items.append(endLiveItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(title: title, items: items, cancelText: .cancelText))))
    }
    
    private func leaveRoom() {
        seatGridView.leaveVoiceRoom { [weak self] in
            guard let self = self else { return }
            manager.onLeaveVoiceRoom()
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code, message: message)
            manager.onError(error.localizedMessage)
        }
    }
}

extension VoiceRoomRootView {
    var deviceStore: DeviceStore {
        return DeviceStore.shared
    }
    
    var liveListStore: LiveListStore {
        return LiveListStore.shared
    }
    
    var coGuestStore: CoGuestStore {
        return CoGuestStore.create(liveId: manager.roomState.roomId)
    }
    
    var seatStore: LiveSeatStore {
        return LiveSeatStore.create(liveId: manager.roomState.roomId)
    }
    
    var barrageStore: BarrageStore {
        return BarrageStore.create(liveId: manager.roomState.roomId)
    }
}

// MARK: - SeatGridViewObserver
extension VoiceRoomRootView: SeatGridViewObserver {
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        guard reason != .byLoggedOnOtherDevice else { return }
        let isOwner = manager.userState.selfInfo.userId == manager.coreLiveState.liveOwner.userId
        isOwner ? routeToAnchorView() : routeToAudienceView()
        manager.onError(.kickedOutText)
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) { [weak self] in
            guard let self = self else { return }
            routerManager.router(action: .exit)
        }
    }
    
    func onKickedOffSeat(userInfo: TUIUserInfo) {
        manager.onError(.onKickedOutOfSeatText)
    }
    
    func onRoomDismissed(roomId: String) {
        if isOwner {
            routeToAnchorView()
            showAnchorEndView()
        } else {
            routeToAudienceView()
            showAudienceEndView()
        }
    }
    
    func onSeatRequestReceived(type: SGRequestType, userInfo: TUIUserInfo) {
        if type == .inviteToTakeSeat {
            let liveOwner = liveListStore.state.value.currentLive.liveOwner
            guard !userInfo.userId.isEmpty else { return }
            guard !liveOwner.userId.isEmpty else { return }
            let alertInfo = VRAlertInfo(description: String.localizedReplace(.inviteLinkText, replace: "\(liveOwner.userName)"),
                                        imagePath: liveOwner.avatarURL,
                                        cancelButtonInfo: (String.rejectText, .g3),
                                        defaultButtonInfo: (String.agreeText, .b1)) { [weak self] _ in
                guard let self = self else { return }
                self.seatGridView.responseRemoteRequest(userId: userInfo.userId, agree: false) { [weak self] in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                } onError: { [weak self] code, message in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                    let error = InternalError(code: code, message: message)
                    manager.onError(error.localizedMessage)
                }
            } defaultClosure: { [weak self] _ in
                guard let self = self else { return }
                self.seatGridView.responseRemoteRequest(userId: userInfo.userId, agree: true) { [weak self] in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                } onError: { [weak self] code, message in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                    let error = InternalError(code: code, message: message)
                    manager.onError(error.localizedMessage)
                }
            }
            routerManager.router(action: .present(.alert(info: alertInfo)))
        } else {
            guard !userInfo.userId.isEmpty else { return }
            manager.onApplyToTakeSeatRequestReceived(userInfo: userInfo)
        }
    }
    
    func onSeatRequestCancelled(type: SGRequestType, userInfo: TUIUserInfo) {
        if type == .inviteToTakeSeat {
            routerManager.router(action: .dismiss(.alert))
        } else {
            manager.onApplyToTakeSeatRequestCancelled(userInfo: userInfo)
        }
    }
    
    func onSeatViewClicked(seatView: UIView, seatInfo: TUISeatInfo) {
        let menus = generateOperateSeatMenuData(seat: seatInfo)
        if menus.isEmpty {
            return
        }
        
        let data = ActionPanelData(items: menus, cancelText: .cancelText)
        routerManager.router(action: .present(.listMenu(data)))
    }
}

// MARK: - Invite/Lock seat
extension VoiceRoomRootView {
    private func generateOperateSeatMenuData(seat: TUISeatInfo) -> [ActionItem] {
        if isOwner {
            return generateRoomOwnerOperateSeatMenuData(seat: seat)
        } else {
            return generateNormalUserOperateSeatMenuData(seat: seat)
        }
    }
    
    private func generateRoomOwnerOperateSeatMenuData(seat: TUISeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        if (seat.userId ?? "").isEmpty {
            if !seat.isLocked {
                let inviteTakeSeat = ActionItem(title: String.inviteText, designConfig: designConfig())
                inviteTakeSeat.actionClosure = { [weak self] _ in
                    guard let self = self else { return }
                    routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                        guard let self = self else { return }
                        routerManager.router(action: .present(.linkInviteControl(seatGridView, seat.index)))
                    }))
                }
                menus.append(inviteTakeSeat)
            }
            
            let lockSeatItem = ActionItem(title: seat.isLocked ? String.unLockSeat : String.lockSeat, designConfig: designConfig())
            lockSeatItem.actionClosure = { [weak self] _ in
                guard let self = self else { return }
                lockSeat(seat: seat)
                routerManager.router(action: .dismiss())
            }
            menus.append(lockSeatItem)
            return menus
        }
        
        let isSelf = seat.userId == manager.userState.selfInfo.userId
        if !isSelf {
            routerManager.router(action: .present(.userControl(seatGridView, seat)))
        }
        return menus
    }
    
    private func generateNormalUserOperateSeatMenuData(seat: TUISeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        let isOnSeat = manager.coreSeatState.seatList.contains { $0.userInfo.userId == manager.userState.selfInfo.userId}
        if (seat.userId ?? "").isEmpty && !seat.isLocked {
            let takeSeatItem = ActionItem(title: .takeSeat, designConfig: designConfig())
            takeSeatItem.actionClosure = { [weak self] _ in
                guard let self = self else { return }
                if isOnSeat {
                    moveToSeat(index: seat.index)
                } else {
                    takeSeat(index: seat.index)
                }
                routerManager.router(action: .dismiss())
            }
            menus.append(takeSeatItem)
            return menus
        }
        
        if !(seat.userId ?? "").isEmpty && seat.userId != manager.userState.selfInfo.userId {
            routerManager.router(action: .present(.userControl(seatGridView, seat)))
        }
        return menus
    }
    
    private func designConfig() -> ActionItemDesignConfig {
        let designConfig = ActionItemDesignConfig(lineWidth: 1, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        return designConfig
    }
    
    private func lockSeat(seat: TUISeatInfo) {
        let lockSeat = TUISeatLockParams()
        lockSeat.lockAudio = seat.isAudioLocked
        lockSeat.lockVideo = seat.isVideoLocked
        lockSeat.lockSeat = !seat.isLocked
        seatGridView.lockSeat(index: seat.index, lockMode: lockSeat) {
  
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code, message: message)
            manager.onError(error.localizedMessage)
        }
    }
    
    private func takeSeat(index: Int) {
        if manager.seatState.isApplyingToTakeSeat {
            makeToast(.repeatRequest)
            return
        }
        manager.onSentTakeSeatRequest()
        seatGridView.takeSeat(index: index, timeout: kSGDefaultTimeout) { [weak self] _ in
            guard let self = self else { return }
            manager.onRespondedTakeSeatRequest()
        } onRejected: { [weak self] userInfo in
            guard let self = self else { return }
            manager.onRespondedTakeSeatRequest()
            makeToast(.takeSeatApplicationRejected)
        } onCancelled: { [weak self] userInfo in
            guard let self = self else { return }
            manager.onRespondedTakeSeatRequest()
        } onTimeout: { [weak self] userInfo in
            guard let self = self else { return }
            manager.onRespondedTakeSeatRequest()
            makeToast(.takeSeatApplicationTimeout)
        } onError: { [weak self] userInfo, code, message in
            guard let self = self else { return }
            manager.onRespondedTakeSeatRequest()
            let error = InternalError(code: code, message: message)
            makeToast(error.localizedMessage)
        }
    }
    
    private func moveToSeat(index: Int) {
        seatGridView.moveToSeat(index: index) {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code, message: message)
            makeToast(error.localizedMessage)
        }
    }
}

// MARK: - BarrageStreamViewDelegate

extension VoiceRoomRootView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: Barrage) -> UIView? {
        guard let type = barrage.extensionInfo?["TYPE"], type == "GIFTMESSAGE" else {
            return nil
        }
        return GiftBarrageCell(barrage: barrage)
    }
    
    func onBarrageClicked(user: LiveUserInfo) {
    }
}

// MARK: - GiftPlayViewDelegate

extension VoiceRoomRootView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: Gift, giftCount: Int, sender: LiveUserInfo) {
        let receiver = TUIUserInfo()
        receiver.userId = manager.coreLiveState.liveOwner.userId
        receiver.userName = manager.coreLiveState.liveOwner.userName
        receiver.avatarUrl = manager.coreLiveState.liveOwner.avatarURL
        if receiver.userId == TUILogin.getUserID() {
            receiver.userName = .meText
        }
        
        var barrage = Barrage()
        barrage.textContent = "gift"
        barrage.sender = sender
        barrage.extensionInfo = [
            "TYPE": "GIFTMESSAGE",
            "gift_name": gift.name,
            "gift_count": "\(giftCount)",
            "gift_icon_url": gift.iconURL,
            "gift_receiver_username": receiver.userName
        ]
        barrageStore.appendLocalTip(message: barrage)
    }
    
    func giftPlayView(_ giftPlayView: GiftPlayView, onPlayGiftAnimation gift: Gift) {
        guard let url = URL(string: gift.resourceURL) else { return }
        giftCacheService.request(withURL: url) { error, fileUrl in
            if error == 0 {
                DispatchQueue.main.async {
                    giftPlayView.playGiftAnimation(playUrl: fileUrl)
                }
            }
        }
    }
}

// MARK: - String
fileprivate extension String {
    static let meText = internalLocalized("Me")
    static let confirmCloseText = internalLocalized("End Live")
    static let rejectText = internalLocalized("Reject")
    static let agreeText = internalLocalized("Agree")
    static let inviteLinkText = internalLocalized("xxx invites you to take seat")
    static let enterRoomFailedText = internalLocalized("Failed to enter room")
    static let inviteText = internalLocalized("Invite")
    static let lockSeat = internalLocalized("Lock Seat")
    static let takeSeat = internalLocalized("Take Seat")
    static let unLockSeat = internalLocalized("Unlock Seat")
    static let operationSuccessful = internalLocalized("Operation Successful")
    static let takeSeatApplicationRejected = internalLocalized("Take seat application has been rejected")
    static let takeSeatApplicationTimeout = internalLocalized("Take seat application timeout")
    static let repeatRequest = internalLocalized("Signal request repetition")
    static let onKickedOutOfSeatText = internalLocalized("Kicked out of seat by room owner")
    static let exitLiveOnLinkMicText = internalLocalized("You are currently co-guesting with other streamers. Would you like to [End Co-guest] or [Exit Live] ?")
    static let exitLiveLinkMicDisconnectText = internalLocalized("End Co-guest")
    static let exitLiveText = internalLocalized("Exit Live")
    static let kickedOutText = internalLocalized("You have been kicked out of the room")
    static let cancelText = internalLocalized("Cancel")
}
