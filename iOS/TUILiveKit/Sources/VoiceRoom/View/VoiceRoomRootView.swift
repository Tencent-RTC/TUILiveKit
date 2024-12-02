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
import SeatGridView

protocol VoiceRoomRootViewDelegate: AnyObject {
    func rootView(_ view: VoiceRoomRootView, showEndView endInfo: [String:Any], isAnchor: Bool)
}

class VoiceRoomRootView: RTCBaseView {
    weak var delegate: VoiceRoomRootViewDelegate?
    
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private let kTimeoutValue: TimeInterval = 60
    private let isOwner: Bool
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    private var cancellableSet = Set<AnyCancellable>()
    
    private let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()
    
    private let backgroundGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    private lazy var topView: VRTopView = {
        let view = VRTopView(manager: manager, routerManager: routerManager)
        return view
    }()
    
    private lazy var seatGridView: SeatGridView = {
        let view = SeatGridView()
        return view
    }()
    
    private lazy var bottomMenu: VRBottomMenuView = {
        let view = VRBottomMenuView(manager: manager, routerManager: routerManager, coreView: seatGridView, isOwner: isOwner)
        view.delegate = self
        return view
    }()
    
    private let muteMicrophoneButton: UIButton = {
        let button = UIButton(frame: .zero)
        button.setImage(UIImage(named: "live_open_mic_icon", in: .liveBundle, with: nil), for: .normal)
        button.setImage(UIImage(named: "live_close_mic_icon", in: .liveBundle, with: nil), for: .selected)
        button.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        button.layer.borderWidth = 1
        button.layer.cornerRadius = 16.scale375Height()
        return button
    }()
    
    private lazy var barrageButton: BarrageInputView = {
        let ownerId = manager.roomState.ownerInfo.userId
        let view = BarrageInputView(roomId: manager.roomState.roomId, ownerId: ownerId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 1
        view.layer.cornerRadius = 18.scale375Height()
        view.backgroundColor = .clear
        return view
    }()
    
    private lazy var barrageDisplayView: BarrageStreamView = {
        let ownerId = manager.roomState.ownerInfo.userId
        let view = BarrageStreamView(roomId: manager.roomState.roomId, ownerId: ownerId)
        view.delegate = self
        return view
    }()
    
    private lazy var giftDisplayView: GiftPlayView = {
        let view = GiftPlayView(groupId: manager.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    lazy var giftListView: GiftListView = {
        let view = GiftListView(groupId: manager.roomState.roomId)
        view.delegate = self
        view.setGiftList(TUIGiftStore.shared.giftList)
        TUIGiftStore.shared.giftCloudServer.queryBalance { error, balance in
            if error == .noError {
                view.setBalance(balance)
            }
        }
        return view
    }()
    
    init(frame: CGRect,
         roomId: String,
         manager: VoiceRoomManager,
         routerManager: VRRouterManager,
         isCreate: Bool) {
        self.manager = manager
        self.routerManager = routerManager
        self.isOwner = isCreate
        super.init(frame: frame)
        manager.update(roomId: roomId)
        if isCreate {
            start(roomId: roomId)
        } else {
            join(roomId: roomId)
        }
        seatGridView.addObserver(observer: self)
    }
    
    deinit {
        seatGridView.removeObserver(observer: self)
        print("deinit \(type(of: self))")
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        backgroundGradientView.gradient(colors: [.g1, .g1.withAlphaComponent(0.5), .g1,], isVertical: true)
    }
    
    override func constructViewHierarchy() {
        addSubview(backgroundImageView)
        addSubview(backgroundGradientView)
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(seatGridView)
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
            make.height.equalTo(230.scale375Height())
            make.left.equalToSuperview()
            make.right.equalToSuperview()
        }
        bottomMenu.snp.makeConstraints { make in
            make.bottom.equalToSuperview().offset(-34.scale375Height())
            make.trailing.equalToSuperview()
            make.height.equalTo(36)
        }
        barrageDisplayView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.bottom.equalTo(bottomMenu.snp.top).offset(8)
            make.width.equalTo(247.scale375())
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
        subscribeSeatState()
        muteMicrophoneButton.addTarget(self, action: #selector(muteMicrophoneButtonClick(sender:)), for: .touchUpInside)
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
            manager.update(microphoneMuted: true)
        } else {
            seatGridView.unmuteMicrophone(onSuccess: { [weak self] in
                guard let self = self else { return }
                manager.update(microphoneMuted: false)
            }, onError: { [weak self] code, message in
                guard let self = self else { return }
                manager.update(microphoneMuted: true)
                guard let err = TUIError(rawValue: code) else { return }
                let error = InternalError(error: err, message: message)
                makeToast(error.localizedMessage)
            })
        }
    }
    
    func startMicrophone() {
        seatGridView.startMicrophone { [weak self] in
            guard let self = self else { return }
            manager.update(microphoneOpened: true)
        } onError: { [weak self] code, message in
            if code == TUIError.openMicrophoneNeedSeatUnlock.rawValue {
                // Seat muted will pops up in unmuteMicrophone, so no processing is needed here
                return
            }
            guard let self = self, let err = TUIError(rawValue: code) else { return }
            let error = InternalError(error: err, message: message)
            makeToast(error.localizedMessage)
        }
    }
    
    func stopMicrophone() {
        seatGridView.stopMicrophone()
        manager.update(microphoneOpened: false)
    }
}

extension VoiceRoomRootView {
    private func start(roomId: String) {
        let roomInfo = TUIRoomInfo()
        let roomState = manager.roomState
        roomInfo.roomId = roomId
        roomInfo.name = roomState.roomName
        roomInfo.seatMode = roomState.seatMode
        roomInfo.maxSeatCount = roomState.maxSeatCount
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        
        seatGridView.startVoiceRoom(roomInfo: roomInfo) { [weak self] roomInfo in
            guard let self = self else { return }
            manager.update(ownerInfo: manager.userState.selfInfo)
            let liveInfo = TUILiveInfo()
            liveInfo.roomInfo.roomId = roomState.roomId
            liveInfo.coverUrl = roomState.coverURL
            liveInfo.backgroundUrl = roomState.backgroundURL
            liveInfo.categoryList = [NSNumber(value: roomState.liveExtraInfo.category.rawValue)]
            liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode == .public
            manager.setLiveInfo(liveInfo: liveInfo, modifyFlag: [.coverUrl, .publish, .category, .backgroundUrl])
            manager.setRoomSeatModeByAdmin(manager.roomState.seatMode)
            
            self.didEnterRoom(roomInfo: roomInfo)
            self.initComponentView()
        } onError: {  [weak self] code, message in
            guard let self = self else { return }
            manager.toastSubject.send(.enterRoomFailedText)
        }
    }
    
    private func join(roomId: String) {
        
        seatGridView.joinVoiceRoom(roomId: roomId) { [weak self] roomInfo in
            guard let self = self else { return }
            self.didEnterRoom(roomInfo: roomInfo)
            self.initComponentView()
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            manager.toastSubject.send(.enterRoomFailedText)
            routerManager.router(action: .exit)
        }
    }
    
    func didEnterRoom(roomInfo: TUIRoomInfo) {
        subscribeMicrophoneState()
        manager.fetchUserList()
        manager.fetchSeatList()
        manager.fetchRoomOwnerInfo(ownerId: roomInfo.ownerId)
        manager.update(roomInfo: roomInfo)
        
        if !isOwner {
            manager.fetchLiveInfo(roomId: manager.roomState.roomId)
            manager.checkFollowType(roomInfo.ownerId)
        }
        
        TUICore.notifyEvent(TUICore_PrivacyService_ROOM_STATE_EVENT_CHANGED,
                            subKey: TUICore_PrivacyService_ROOM_STATE_EVENT_SUB_KEY_START,
                            object: nil,
                            param: nil)
    }
    
    func initComponentView() {
        initTopView()
    }
    
    func initTopView() {
        topView.initialize(roomId: manager.roomState.roomId)
    }
}

// MARK: - EndView

extension VoiceRoomRootView {
    private func showAnchorEndView() {
        seatGridView.stopVoiceRoom { [weak self] in
            guard let self = self else { return }
            manager.update(microphoneOpened: false)
        } onError: { [weak self] code, message in
            guard let self = self, let err = TUIError(rawValue: code) else { return }
            let error = InternalError(error: err, message: message)
            manager.toastSubject.send(error.localizedMessage)
        }
        
        let roomState = manager.roomState
        let giftIncome = roomState.liveExtraInfo.giftIncome
        let giftPeopleCount = roomState.liveExtraInfo.giftPeopleSet.count
        let audienceCount = manager.userState.userList.count
        let liveDataModel = LiveDataModel(roomId: manager.roomState.roomId,
                                          liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                          audienceCount: audienceCount == 0 ? 0 : audienceCount - 1,
                                          messageCount: barrageDisplayView.getBarrageCount(),
                                          giftIncome: giftIncome,
                                          giftPeopleCount: giftPeopleCount,
                                          likeCount: giftDisplayView.getLikeCount())
        delegate?.rootView(self, showEndView: ["data": liveDataModel], isAnchor: true)
    }
    
    private func showAudienceEndView() {
        if !isOwner {
            let info: [String: Any] = [
                "roomId": manager.roomState.roomId,
                "avatarUrl": manager.roomState.ownerInfo.avatarUrl,
                "userName": manager.userState.selfInfo.name
            ]
            delegate?.rootView(self, showEndView: info, isAnchor: false)
        }
    }
}

// MARK: - RouterViewProvider

extension VoiceRoomRootView: VRRouterViewProvider {
    func getRouteView(route: VRRoute) -> UIView? {
        if route == .giftView {
            giftListView.setGiftList(TUIGiftStore.shared.giftList)
            return giftListView
        } else {
            return nil
        }
    }
}

// MARK: - Private

extension VoiceRoomRootView {
    private func subscribeRoomState() {
        subscribeRoomBackgroundState()
        subscribeRoomIdState()
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
        manager.subscribeRoomState(StateSelector(keyPath: \.backgroundURL))
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: URL(string: url), placeholder: UIImage.placeholderImage)
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomIdState() {
        manager.subscribeRoomState(StateSelector(keyPath: \.roomId))
            .receive(on: RunLoop.main)
            .sink { [weak self] roomId in
                guard let self = self else { return }
                if !roomId.isEmpty {
                    self.barrageButton.setRoomId(roomId: roomId)
                    self.barrageDisplayView.setRoomId(roomId: roomId)
                    self.giftListView.setRoomId(roomId: roomId)
                    self.giftDisplayView.setRoomId(roomId: roomId)
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomOwnerState() {
        manager.subscribeRoomState(StateSelector(keyPath: \.ownerInfo))
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageButton.setOwnerId(ownerId: ownerInfo.userId)
                self.barrageDisplayView.setOwnerId(ownerId: ownerInfo.userId)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - SubscribeUserState

extension VoiceRoomRootView {
    private func subscribeUserIsOnSeatState() {
        let selfInfoPublisher = manager.subscribeUserState(StateSelector(keyPath: \.selfInfo))
        let seatListPublisher = manager.subscribeSeatState(StateSelector(keyPath: \.seatList))
        seatListPublisher
            .combineLatest(selfInfoPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList, selfInfo in
                guard let self = self else { return }
                muteMicrophoneButton.isHidden = !(seatList.contains { $0.userId == selfInfo.userId })
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeUserLinkState() {
        manager.subscribeUserState(StateSelector(keyPath: \.selfInfo.linkStatus))
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

// MARK: - SubscribeViewState

extension VoiceRoomRootView {
    private func subscribeMicrophoneState() {
        manager.subscribeMediaState(StateSelector(keyPath: \.isMicrophoneMuted))
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] microphoneMuted in
                guard let self = self else { return }
                self.muteMicrophoneButton.isSelected = microphoneMuted
            })
            .store(in: &cancellableSet)
    }
}

// MARK: - SubscribeSeatState

extension VoiceRoomRootView {
    private func subscribeSeatState() {
        manager.subscribeSeatState(StateSelector(keyPath: \.seatList))
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList in
                guard let self = self else { return }
                var linkStatus: LinkStatus = .none
                if seatList.contains(where: { $0.userId == self.manager.userState.selfInfo.userId }) {
                    linkStatus = .linking
                }
                manager.update(linkStatus: linkStatus)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - TopViewDelegate

extension VoiceRoomRootView: VRTopViewDelegate {
    func topView(_ topView: VRTopView, tap event: VRTopView.TapEvent, sender: Any?) {
        switch event {
        case .stop:
            if isOwner {
                let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
                designConfig.backgroundColor = .white
                designConfig.lineColor = .g8
                let item = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
                    guard let self = self else { return }
                    manager.update(linkStatus: .none)
                    self.showAnchorEndView()
                    self.routerManager.router(action: .dismiss())
                })
                routerManager.router(action: .present(.listMenu(ActionPanelData(items: [item]))))
            } else {
                manager.update(linkStatus: .none)
                seatGridView.leaveVoiceRoom { [weak self] in
                    guard let self = self else { return }
                    manager.resetAllState()
                } onError: { [weak self] code, message in
                    guard let self = self, let err = TUIError(rawValue: code) else { return }
                    let error = InternalError(error: err, message: message)
                    manager.toastSubject.send(error.localizedMessage)
                }
                routerManager.router(action: .exit)
            }
        case .roomInfo:
            routerManager.router(action: .present(.roomInfo))
        case .audienceList:
            routerManager.router(action: .present(.recentViewer))
        }
    }
}

// MARK: - VRBottomMenuViewDelegate
extension VoiceRoomRootView: VRBottomMenuViewDelegate {
    func likeButtonClicked() {
        giftListView.sendLike()
    }
}

// MARK: - SeatGridViewObserver
extension VoiceRoomRootView: SeatGridViewObserver {
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
    }
    
    func onKickedOffSeat(userInfo: TUIUserInfo) {
    }
    
    func onUserAudioStateChanged(userInfo: TUIUserInfo, hasAudio: Bool, reason: TUIChangeReason) {
        if userInfo.userId == manager.userState.selfInfo.userId {
            manager.update(microphoneMuted: !hasAudio)
        }
    }
    
    func onRoomDismissed(roomId: String) {
        showAudienceEndView()
    }
    
    func onSeatRequestReceived(type: SGRequestType, userInfo: TUIUserInfo) {
        if type == .inviteToTakeSeat {
            guard !userInfo.userId.isEmpty else { return }
            let alertInfo = VRAlertInfo(description: String.localizedReplace(.inviteLinkText, replace: "\(userInfo.userName)"),
                                        imagePath: userInfo.avatarUrl,
                                        cancelButtonInfo: (String.rejectText, .g3),
                                        defaultButtonInfo: (String.acceptText, .b1)) { [weak self] _ in
                guard let self = self else { return }
                self.seatGridView.responseRemoteRequest(userId: userInfo.userId, agree: false) { [weak self] in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                } onError: { [weak self] code, message in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                    guard let err = TUIError(rawValue: code) else { return }
                    let error = InternalError(error: err, message: message)
                    manager.toastSubject.send(error.localizedMessage)
                }
            } defaultClosure: { [weak self] _ in
                guard let self = self else { return }
                self.seatGridView.responseRemoteRequest(userId: userInfo.userId, agree: true) { [weak self] in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                } onError: { [weak self] code, message in
                    guard let self = self else { return }
                    self.routerManager.router(action: .dismiss(.alert))
                    guard let err = TUIError(rawValue: code) else { return }
                    let error = InternalError(error: err, message: message)
                    manager.toastSubject.send(error.localizedMessage)
                }
            }
            routerManager.router(action: .present(.alert(info: alertInfo)))
        } else {
            guard !userInfo.userId.isEmpty else { return }
            manager.addSeatUserInfo(userInfo)
        }
    }
    
    func onSeatRequestCancelled(type: SGRequestType, userInfo: TUIUserInfo) {
        if type == .inviteToTakeSeat {
            routerManager.router(action: .dismiss(.alert))
        } else {
            manager.removeSeatUserInfo(userInfo)
        }
    }
    
    func onSeatViewClicked(seatView: UIView, seatInfo: TUISeatInfo) {
        let menus = generateOperateSeatMenuData(seat: VRSeatInfo(info: seatInfo))
        if menus.isEmpty {
            return
        }
        
        let data = ActionPanelData(items: menus)
        routerManager.router(action: .present(.listMenu(data)))
    }
}

// MARK: - Invite/Lock seat
extension VoiceRoomRootView {
    private func generateOperateSeatMenuData(seat: VRSeatInfo) -> [ActionItem] {
        if isOwner {
            return generateRoomOwnerOperateSeatMenuData(seat: seat)
        } else {
            return generateNormalUserOperateSeatMenuData(seat: seat)
        }
    }
    
    private func generateRoomOwnerOperateSeatMenuData(seat: VRSeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        if seat.userId.isEmpty {
            if !seat.isLocked {
                let inviteTakeSeat = ActionItem(title: String.inviteText, designConfig: designConfig())
                inviteTakeSeat.actionClosure = { [weak self] _ in
                    guard let self = self else { return }
                    routerManager.router(action: .present(.linkInviteControl(seatGridView, seat.index)))
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
    
    private func generateNormalUserOperateSeatMenuData(seat: VRSeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        let isOnSeat = manager.seatState.seatList.contains { $0.userId == manager.userState.selfInfo.userId}
        if seat.userId.isEmpty && !seat.isLocked {
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
        
        if !seat.userId.isEmpty && seat.userId != manager.userState.selfInfo.userId {
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
    
    private func lockSeat(seat: VRSeatInfo) {
        let lockSeat = TUISeatLockParams()
        lockSeat.lockAudio = seat.isAudioLocked
        lockSeat.lockVideo = seat.isVideoLocked
        lockSeat.lockSeat = !seat.isLocked
        seatGridView.lockSeat(index: seat.index, lockMode: lockSeat) { [weak self] in
            guard let self = self else { return }
            makeToast(.operationSuccessful)
        } onError: { [weak self] code, message in
            guard let self = self, let err = TUIError(rawValue: code) else { return }
            let error = InternalError(error: err, message: message)
            manager.toastSubject.send(error.localizedMessage)
        }
    }
    
    private func takeSeat(index: Int) {
        if manager.seatState.isApplyingToTakeSeat {
            makeToast(.repeatRequest)
            return
        }
        seatGridView.takeSeat(index: index, timeout: kSGDefaultTimeout) { [weak self] _ in
            guard let self = self else { return }
            handleApplicationState(isApplying: false)
        } onRejected: { [weak self] userInfo in
            guard let self = self else { return }
            handleApplicationState(isApplying: false)
            makeToast(.takeSeatApplicationRejected)
        } onCancelled: { [weak self] userInfo in
            guard let self = self else { return }
            handleApplicationState(isApplying: false)
        } onTimeout: { [weak self] userInfo in
            guard let self = self else { return }
            handleApplicationState(isApplying: false)
            makeToast(.takeSeatApplicationTimeout)
        } onError: { [weak self] userInfo, code, message in
            guard let self = self else { return }
            handleApplicationState(isApplying: false)
            guard let err = TUIError(rawValue: code) else { return }
            let error = InternalError(error: err, message: message)
            makeToast(error.localizedMessage)
        }
        handleApplicationState(isApplying: true)
    }
    
    private func moveToSeat(index: Int) {
        seatGridView.moveToSeat(index: index) {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            guard let err = TUIError(rawValue: code) else { return }
            let error = InternalError(error: err, message: message)
            makeToast(error.localizedMessage)
        }
    }
    
    private func handleApplicationState(isApplying: Bool) {
        manager.update(applicationStateIsApplying: isApplying)
    }
}

// MARK: - BarrageStreamViewDelegate

extension VoiceRoomRootView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell(barrage: barrage)
    }
}

// MARK: - GiftListViewDelegate

extension VoiceRoomRootView: GiftListViewDelegate {
    func onRecharge(giftListView view: GiftListView) {
        TUIGiftStore.shared.giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                manager.toastSubject.send(.balanceInsufficientText)
            }
        }
    }
    
    func onSendGift(giftListView view: GiftListView, giftModel: TUIGift, giftCount: Int) {
        
        let anchorInfo = manager.roomState.ownerInfo
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        let selfInfo = manager.userState.selfInfo
        TUIGiftStore.shared.giftCloudServer.sendGift(sender: selfInfo.userId,
                                                     receiver: receiver.userId,
                                                     giftModel: giftModel,
                                                     giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                giftListView.makeToast(.balanceInsufficientText)
            }
        }
    }
}

// MARK: - GiftPlayViewDelegate

extension VoiceRoomRootView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let userId = manager.userState.selfInfo.userId
        if isOwner && userId == receiver.userId {
            manager.update(giftIncome: gift.price * giftCount, giftPeople: sender.userId)
        }
        let barrage = TUIBarrage()
        barrage.content = "gift"
        barrage.user.userId = sender.userId
        barrage.user.userName = sender.userName
        barrage.user.avatarUrl = sender.avatarUrl
        barrage.user.level = sender.level
        barrage.extInfo["TYPE"] = AnyCodable("GIFTMESSAGE")
        barrage.extInfo["gift_name"] = AnyCodable(gift.giftName)
        barrage.extInfo["gift_count"] = AnyCodable(giftCount)
        barrage.extInfo["gift_icon_url"] = AnyCodable(gift.imageUrl)
        if receiver.userId == TUILogin.getUserID() {
            receiver.userName = .meText
        }
        barrage.extInfo["gift_receiver_username"] = AnyCodable(receiver.userName)
        barrageDisplayView.insertBarrages([barrage])
    }
    
    func giftPlayView(_ giftPlayView: GiftPlayView, onPlayGiftAnimation gift: TUIGift) {
        guard let url = URL(string: gift.animationUrl) else { return }
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
    static let meText = localized("live.barrage.me")
    static let confirmCloseText = localized("live.anchor.confirm.close")
    static let balanceInsufficientText = localized("live.balanceInsufficient")
    static let rejectText = localized("live.anchor.link.reject.title")
    static let acceptText = localized("live.anchor.link.accept.title")
    static let inviteLinkText = localized("live.anchor.link.invite.desc.xxx")
    static let enterRoomFailedText = localized("live.alert.enterRoom.failed.title")
    static let operationFailedText = localized("live.operation.fail.xxx")
    static let inviteText = localized("live.seat.invite")
    static let lockSeat = localized("live.seat.lockSeat")
    static let takeSeat = localized("live.seat.takeSeat")
    static let unLockSeat = localized("live.seat.unLockSeat")
    static let operationSuccessful = localized("live.error.success")
    static let takeSeatSuccess = localized("live.seat.takeSeatSuccess")
    static let takeSeatApplicationRejected = localized("live.seat.takeSeatApplicationRejected")
    static let takeSeatApplicationTimeout = localized("live.seat.takeSeatApplicationTimeout")
    static let repeatRequest = localized("live.error.repeat.requestId")
}
