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

class VoiceRoomRootView: RTCBaseView {
    
    let roomId: String
    let store: LiveStoreProvider
    let viewStore: VoiceRoomViewStore
    let routerStore: RouterStore
    
    private lazy var isOwner: Bool = store.selectCurrent(UserSelectors.isOwner)
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    private var cancellableSet = Set<AnyCancellable>()
    private weak var alertPanel: AlertPanel?
    
    let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()
    
    private let backgroundGradientView: UIView = {
        var view = UIView()
        return view
    }()
    
    private lazy var topView: TopView = {
        let view = TopView(store: store, routerStore: routerStore)
        return view
    }()
    
    private lazy var seatListView: SeatListView = {
        let seatCount = store.selectCurrent(SeatSelectors.getSeatCount)
        let view = SeatListView(seatCount: seatCount)
        return view
    }()
    
    private lazy var bottomMenu: BottomMenuView = {
        let view = BottomMenuView(frame: .zero)
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
    
    private lazy var barrageButton: TUIBarrageButton = {
        let ownerId = self.store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageButton(roomId: roomId, ownerId: ownerId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 1
        view.layer.cornerRadius = 18.scale375Height()
        view.backgroundColor = .clear
        return view
    }()
    
    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let ownerId = self.store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageDisplayView(roomId: roomId, ownerId: ownerId)
        view.delegate = self
        return view
    }()
    
    private lazy var giftDisplayView: TUIGiftPlayView = {
        let view = TUIGiftPlayView(groupId: roomId)
        view.delegate = self
        return view
    }()
    
    lazy var giftListView: TUIGiftListView = {
        let view = TUIGiftListView(groupId: roomId)
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
         routerStore: RouterStore,
         isCreate: Bool) {
        self.roomId = roomId
        self.routerStore = routerStore
        self.store = LiveStoreFactory.getStore(roomId: roomId)
        self.viewStore = VoiceRoomViewStoreFactory.getStore(roomId: roomId)
        super.init(frame: frame)
        store.dispatch(action: UserActions.getSelfInfo())
        store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
        if isCreate {
            start()
        } else {
            join()
        }
    }
    
    deinit {
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
        addSubview(seatListView)
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
        seatListView.snp.makeConstraints { make in
            make.top.equalTo(topView.snp.bottom).offset(40.scale375())
            make.height.equalTo(seatListView.getHeight())
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
        seatListView.delegate = self
        subscribeRoomState()
        subscribeSeatState()
        subscribeUserState()
        subscribeViewState()
        muteMicrophoneButton.addTarget(self, action: #selector(muteMicrophoneButtonClick(sender:)), for: .touchUpInside)
    }
}

extension VoiceRoomRootView {
    @objc
    func muteMicrophoneButtonClick(sender: UIButton) {
        store.dispatch(action: MediaActions.operateMicrophoneMute(payload: !sender.isSelected))
    }
}

extension VoiceRoomRootView {
    private func start() {
        let roomInfo = TUIRoomInfo()
        let roomState = store.selectCurrent(RoomSelectors.getRoomState)
        roomInfo.roomId = roomState.roomId
        roomInfo.name = roomState.roomName
        roomInfo.seatMode = roomState.seatMode
        roomInfo.maxSeatCount = roomState.maxSeatCount
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        let config = generateActionParamTuple(param: roomInfo, actions: [])
        DataReporter.componentType = .voiceRoom
        store.dispatch(action: RoomActions.start(payload: config))
    }
    
    private func join() {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let param = generateActionParamTuple(param: roomId, actions: [])
        DataReporter.componentType = .voiceRoom
        store.dispatch(action: ViewActions.updateLiveStatus(payload: .previewing))
        store.dispatch(action: RoomActions.join(payload: param))
    }
    
    func didEnterRoom() {
        subscribeMicrophoneState()
        var actions:[Action] = [
            SeatActions.fetchSeatList(),
            UserActions.fetchUserList(),
            RoomActions.fetchRoomOwnerInfo(payload: store.selectCurrent(RoomSelectors.roomOwnerId)),
            ViewActions.updateAutoOpenCameraOnSeated(payload: false),
        ]
        if isOwner {
            actions.append(SeatActions.takeSeat(payload: nil))
            actions.append(MediaActions.operateMicrophone(payload: true))
        } else {
            actions.append(RoomActions.fetchLiveInfo(payload: roomId))
            actions.append(UserActions.checkFollowType(payload: store.selectCurrent(RoomSelectors.roomOwnerId)))
        }
        actions.forEach { action in
            store.dispatch(action: action)
        }
        viewStore.dispatch(action: VoiceRoomViewActions.updateBottomMenus(payload: (store, routerStore, viewStore)))
    }
}

extension VoiceRoomRootView {
    private func showAnchorEndView() {
        store.dispatch(action: RoomActions.stop())
        let roomState = store.selectCurrent(RoomSelectors.getRoomState)
        let giftIncome = store.selectCurrent(RoomSelectors.getGiftIncome)
        let giftPeopleCount = store.selectCurrent(RoomSelectors.getGiftPeopleSet).count
        let audienceCount = store.selectCurrent(UserSelectors.getUserList).count
        let liveDataModel = LiveDataModel(roomId: roomId,
                                          liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                          audienceCount: audienceCount == 0 ? 0 : audienceCount - 1,
                                          messageCount: barrageDisplayView.getBarrageCount(),
                                          giftIncome: giftIncome,
                                          giftPeopleCount: giftPeopleCount,
                                          likeCount: giftDisplayView.getLikeCount())
        let anchorEndView = AnchorEndView(liveDataModel: liveDataModel, routerStore: routerStore)
        addSubview(anchorEndView)
        anchorEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func showAudienceEndView() {
        if !isOwner {
            let roomOwner = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
            let audienceEndView = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
            addSubview(audienceEndView)
            audienceEndView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }
}

extension VoiceRoomRootView: RouterViewProvider {
    func getRouteView(route: Route) -> UIView? {
        if route == .giftView {
            giftListView.setGiftList(TUIGiftStore.shared.giftList)
            return giftListView
        } else {
            return nil
        }
    }
}

extension VoiceRoomRootView {
    private func subscribeRoomState() {
        subscribeRoomBackgroundState()
        subscribeRoomIdState()
        subscribeRoomOwnerState()
    }
    
    private func subscribeSeatState() {
        subscribeSeatCountState()
        subscribeReceivedSeatInvitationState()
    }
    
    private func subscribeUserState() {
        subscribeUserIsOnSeatState()
    }
    
    private func subscribeViewState() {
        subscribeViewLiveStatusState()
        subscribeCustomEvent()
        subscribeBottomMenuState()
        subscribeToastState()
        subscribeAlertState()
    }
}

// room
extension VoiceRoomRootView {
    private func subscribeRoomBackgroundState() {
        store.select(RoomSelectors.getRoomBackgroundUrl)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: URL(string: url), placeholder: UIImage.placeholderImage)
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomIdState() {
        store.select(RoomSelectors.getRoomId)
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
        store.select(RoomSelectors.getRoomOwnerInfo)
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageButton.setOwnerId(ownerId: ownerInfo.userId)
                self.barrageDisplayView.setOwnerId(ownerId: ownerInfo.userId)
            }
            .store(in: &cancellableSet)
    }
}

// seat
extension VoiceRoomRootView {
    private func subscribeSeatCountState() {
        store.select(SeatSelectors.getSeatCount)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] count in
                guard let self = self else { return }
                debugPrint("===== count:\(count)")
                self.seatListView.seatCount = count
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeReceivedSeatInvitationState() {
        store.select(SeatSelectors.getReceivedSeatInvitation)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInvitation in
                guard let self = self else { return }
                if !seatInvitation.id.isEmpty {
                    let alertInfo = AlertInfo(description: String.localizedReplace(.inviteLinkText, replace: "\(seatInvitation.userName)"),
                                              imagePath: seatInvitation.avatarUrl,
                                              cancelButtonInfo: (String.rejectText, .g3),
                                              defaultButtonInfo: (String.acceptText, .b1)) { [weak self] _ in
                        guard let self = self else { return }
                        self.store.dispatch(action: SeatActions.responseSeatInvitation(payload: (false, seatInvitation.id)))
                        self.store.dispatch(action: SeatActions.updateReceivedSeatInvitation(payload: SeatInvitation()))
                    } defaultClosure: { [weak self] _ in
                        guard let self = self else { return }
                        self.store.dispatch(action: SeatActions.responseSeatInvitation(payload: (true, seatInvitation.id)))
                        self.store.dispatch(action: SeatActions.updateReceivedSeatInvitation(payload: SeatInvitation()))
                    }
                    store.dispatch(action: ViewActions.alertEvent(payload: alertInfo))
                } else {
                    alertPanel?.dismiss()
                }
            }
            .store(in: &cancellableSet)
    }
}

// user
extension VoiceRoomRootView {
    private func subscribeUserIsOnSeatState() {
        store.select(UserSelectors.isOnSeat)
            .receive(on: RunLoop.main)
            .map { !$0 }
            .assign(to: \UIButton.isHidden, on: muteMicrophoneButton)
            .store(in: &cancellableSet)
    }
}

// view&media
extension VoiceRoomRootView {
    private func subscribeViewLiveStatusState() {
        store.select(ViewSelectors.getLiveStatus)
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                case .playing, .pushing:
                    self.topView.isHidden = false
                    self.bottomMenu.isHidden = false
                    self.seatListView.isHidden = false
                    self.didEnterRoom()
                case .none, .finished:
                    self.topView.isHidden = true
                    self.bottomMenu.isHidden = true
                    self.seatListView.isHidden = true
                default:
                    break
                }
                if status == .finished {
                    showAudienceEndView()
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeMicrophoneState() {
        store.select(MediaSelectors.getMicrophoneMutedState)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] microphoneMuted in
                guard let self = self else { return }
                self.muteMicrophoneButton.isSelected = microphoneMuted
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeCustomEvent() {
        viewStore.viewActionSubject
            .receive(on: DispatchQueue.main)
            .filter({
                $0.id == VoiceRoomViewResponseActions.like.id
            })
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.giftListView.sendLike()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeBottomMenuState() {
        viewStore.select(VoiceRoomViewSelectors.getBottomMenuButtons)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] menus in
                guard let self = self else { return }
                self.bottomMenu.menus = menus
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeToastState() {
        store.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] toast in
                guard let self = self else { return }
                self.makeToast(toast.message, duration: toast.duration, position: toast.position)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeAlertState() {
        store.alertSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] alertInfo in
                guard let self = self else { return }
                let alertPanel = AlertPanel(alertInfo: alertInfo)
                alertPanel.show()
                self.alertPanel = alertPanel
            }
            .store(in: &cancellableSet)
    }
}

extension VoiceRoomRootView: TopViewDelegate {
    func topView(_ topView: TopView, tap event: TopView.TapEvent, sender: Any?) {
        switch event {
        case .stop:
            if isOwner {
                let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
                designConfig.backgroundColor = .white
                designConfig.lineColor = .g8
                let item = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
                    guard let self = self else { return }
                    self.showAnchorEndView()
                    self.routerStore.router(action: .dismiss())
                })
                routerStore.router(action: .present(.listMenu(ActionPanelData(items: [item]))))
            } else {
                store.dispatch(action: RoomActions.leave())
                routerStore.router(action: .exit)
            }
        case .roomInfo:
            routerStore.router(action: .present(.roomInfo))
        case .audienceList:
            routerStore.router(action: .present(.recentViewer))
        }
    }
}

extension VoiceRoomRootView: SeatListViewDelegate {
    func seatListView(_ seatListView: SeatListView, needUpdateViewState seatView: SeatView, at indexPath: IndexPath) {
        // Here you can bind seat&user data for seat view.
        let seatInfoSelector = SeatSelectors.getSeatInfo(index: indexPath.item)
        let seatInfoPublisher = store.select(seatInfoSelector)
        let audioAvailableUsers = store.select(UserSelectors.getHasAudioStreamUserList)
        seatInfoPublisher
            .receive(on: RunLoop.main)
            .sink(receiveValue: {  [weak self, weak seatView] seatInfo in
                guard let self = self else { return }
                seatView?.seatInfo = seatInfo
                seatView?.ownerImageView.isHidden = seatInfo.userId != self.store.selectCurrent(RoomSelectors.roomOwnerId)
            })
            .store(in: &seatView.cancellableSet)
        let fullSeatPublisher = seatInfoPublisher
            .filter { !$0.userId.isEmpty }
            .map { $0.userId }
        audioAvailableUsers
            .combineLatest(fullSeatPublisher)
            .filter { !$0.1.isEmpty }
            .receive(on: RunLoop.main)
            .sink { [weak seatView, weak self] users, _ in
                guard let seatView = seatView, let self = self else { return }
                let seatInfo = self.store.selectCurrent(seatInfoSelector)
                if !seatInfo.userId.isEmpty {
                    seatView.isAudioMuted = !users.contains(seatInfo.userId)
                }
            }
            .store(in: &seatView.cancellableSet)
        let userVolumePublisher = store.select(UserSelectors.getHasAudioVolumeUserList)
        fullSeatPublisher
            .combineLatest(userVolumePublisher)
            .receive(on: RunLoop.main)
            .sink { [weak seatView] userId, users in
                guard let seatView = seatView else { return }
                if !seatView.seatInfo.userId.isEmpty {
                    seatView.isSpeaking = users.contains(userId)
                }
            }
            .store(in: &seatView.cancellableSet)
    }
    
    func seatListView(_ seatListView: SeatListView, didSelectSeatAt index: Int) {
        // Here you can handle the click event of the seat list view.
        let seatInfo = store.selectCurrent(SeatSelectors.getSeatList)[index]
        let menus = VoiceRoomRootMenuDataCreator().generateOperateSeatMenuData(store: store, routerStore: routerStore, seat: seatInfo)
        if menus.count > 0 {
            routerStore.router(action: .present(.listMenu(ActionPanelData(items: menus))))
        }
    }
}

extension VoiceRoomRootView: TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell.getCustomCell(barrage: barrage)
    }
}

extension VoiceRoomRootView: TUIGiftListViewDelegate {
    func onRecharge(giftListView view: TUIGiftListView) {
        TUIGiftStore.shared.giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                let toastInfo = ToastInfo(message: .balanceInsufficientText)
                store.dispatch(action: ViewActions.toastEvent(payload: toastInfo))
            }
        }
    }
    
    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int) {
        
        let anchorInfo = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        let selfInfo = store.selectCurrent(UserSelectors.getSelfInfo)
        TUIGiftStore.shared.giftCloudServer.sendGift(sender: selfInfo.userId,
                                                     receiver: receiver.userId,
                                                     giftModel: giftModel,
                                                     giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                let toastInfo = ToastInfo(message: .balanceInsufficientText)
                store.dispatch(action: ViewActions.toastEvent(payload: toastInfo))
            }
        }
    }
}

extension VoiceRoomRootView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let userId = store.selectCurrent(UserSelectors.currentUserId)
        if isOwner && userId == receiver.userId {
            store.dispatch(action: RoomActions.updateGiftIncome(payload: gift.price * giftCount))
            store.dispatch(action: RoomActions.updateGiftPeople(payload: sender.userId))
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
    
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onPlayGiftAnimation gift: TUIGift) {
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

fileprivate extension String {
    static let meText = localized("live.barrage.me")
    static var confirmCloseText = localized("live.anchor.confirm.close")
    static let balanceInsufficientText = localized("live.balanceInsufficient")
    static let rejectText = localized("live.anchor.link.reject.title")
    static let acceptText = localized("live.anchor.link.accept.title")
    static let inviteLinkText = localized("live.anchor.link.invite.desc.xxx")
}
