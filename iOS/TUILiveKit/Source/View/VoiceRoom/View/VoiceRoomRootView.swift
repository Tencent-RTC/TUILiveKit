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

class VoiceRoomRootView: RTCBaseView {
    
    @Injected var store: LiveStore
    @Injected var viewStore: VoiceRoomViewStore
    
    // TopView Status
    lazy var userCount = self.store.select(RoomSelectors.getUserCount)
    lazy var coverUrl = self.store.select(RoomSelectors.getRoomCoverUrl)
    lazy var memberAvatars = self.store.select(UserSelectors.getMemberAvatars)

    // SeatList Status
    lazy var seatCount = self.store.select(SeatSelectors.getSeatCount)
    lazy var liveStatusPublisher = self.store.select(ViewSelectors.getLiveStatus)
    
    private var cancellableSet = Set<AnyCancellable>()
    private let giftCacheService = GiftCacheService()

    private var isOwner: Bool {
        return store.selectCurrent(UserSelectors.isOwner)
    }
    
    private lazy var roomIdPublisher = self.store.select(RoomSelectors.getRoomId)
    private lazy var ownerInfoPublisher = self.store.select(RoomSelectors.getRoomOwnerInfo)

    let backgroundImageView: UIImageView = {
        let backgroundImageView = UIImageView(frame: .zero)
        backgroundImageView.contentMode = .scaleAspectFill
        return backgroundImageView
    }()

    let backgroundLayer: CALayer = {
        let layer = CAGradientLayer()
        layer.colors = [UIColor.darkNavyColor.cgColor, UIColor.pureBlackColor.cgColor]
        layer.locations = [0.2, 1.0]
        layer.startPoint = CGPoint(x: 0.4, y: 0)
        layer.endPoint = CGPoint(x: 0.6, y: 1.0)
        return layer
    }()

    private let topView: TopView = {
        let view = TopView(frame: .zero)
        return view
    }()

    private let seatListView: SeatListView = {
        let view = SeatListView(frame: .zero)
        return view
    }()

    private lazy var bottomMenu: BottomMenuView = {
        let view = BottomMenuView(frame: .zero)
        let menusPublisher = self.viewStore.select(VoiceRoomViewSelectors.getBottomMenuButtons)
        weak var bottomMenuView = view
        menusPublisher
            .sink(receiveValue: { menus in
                bottomMenuView?.menus = menus
            })
            .store(in: &view.cancellableSet)
        return view
    }()

    private lazy var barrageButton: TUIBarrageButton = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let ownerId = self.store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageButton(roomId: roomId, ownerId: ownerId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        return view
    }()

    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let ownerId = self.store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageDisplayView(roomId: roomId, ownerId: ownerId)
        view.delegate = self
        return view
    }()

    private lazy var giftDisplayView: TUIGiftPlayView = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let view = TUIGiftPlayView(groupId: roomId)
        view.delegate = self
        return view
    }()

    lazy var giftListView: TUIGiftListView = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let view = TUIGiftListView(groupId: roomId)
        view.delegate = self
        let giftCloudServer = GiftCloudServer()
        giftCloudServer.queryGiftInfoList { error, giftList in
            if error == .noError {
                view.setGiftList(giftList)
            }
        }
        view.setBalance(500)
        return view
    }()

    lazy var musicPanelView: MusicPanelView = {
        let view = MusicPanelView(frame: .zero)
        return view
    }()
    
    override func constructViewHierarchy() {
        layer.insertSublayer(backgroundLayer, at: 0)
        addSubview(backgroundImageView)
        addSubview(giftDisplayView)
        addSubview(topView)
        addSubview(seatListView)
        addSubview(bottomMenu)
        addSubview(barrageDisplayView)
        addSubview(barrageButton)
    }
    
    override func activateConstraints() {
        backgroundImageView.snp.makeConstraints { (make) in
            make.edges.equalToSuperview()
        }
        topView.snp.makeConstraints { make in
            make.left.right.equalToSuperview()
            make.top.equalTo(safeAreaLayoutGuide.snp.top).offset(10)
        }
        seatListView.snp.makeConstraints { make in
            make.top.equalTo(topView.snp.bottom).offset(40)
            make.height.equalTo(200)
            make.left.equalToSuperview()
            make.right.equalToSuperview()
        }
        bottomMenu.snp.makeConstraints { make in
            make.bottom.equalTo(safeAreaLayoutGuide.snp.bottom).offset(-20)
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
            make.height.equalTo(36)
        }
        giftDisplayView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        // Top view interaction.
        topView.delegate = self
        subscribeRoomState()
        subscribeTopViewState()
        seatListView.delegate = self
        subscribeSeatState()
        subscribeCustomEvent()
    }
    
    override func setupViewStyle() {}
    
    private func showAnchorEndView() {
        store.dispatch(action: RoomActions.stop())
        let roomId: String = store.selectCurrent(RoomSelectors.getRoomId)
        let roomState = store.selectCurrent(RoomSelectors.getRoomState)
        let giftIncome = store.selectCurrent(RoomSelectors.getGiftIncome)
        let giftPeopleCount = store.selectCurrent(RoomSelectors.getGiftPeopleSet).count
        let audienceCount = store.selectCurrent(UserSelectors.getAudienceUserList).count
        let liveDataModel = LiveDataModel(roomId: roomId,
                                          liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                          audienceCount: audienceCount,
                                          messageCount: barrageDisplayView.getBarrageCount(),
                                          giftIncome: giftIncome,
                                          giftPeopleCount: giftPeopleCount,
                                          likeCount: giftDisplayView.getLikeCount())
        let anchorEndView = AnchorEndView(liveDataModel: liveDataModel)
        addSubview(anchorEndView)
        anchorEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func showAudienceEndView() {
        if !store.selectCurrent(UserSelectors.isOwner) {
            let roomId: String = store.selectCurrent(RoomSelectors.getRoomId)
            let roomOwner = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
            let audienceEndView = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
            addSubview(audienceEndView)
            audienceEndView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }

    deinit {
        giftCacheService.clearCacheDirectory()
        print("deinit \(type(of: self))")
    }
}

extension VoiceRoomRootView {
    private func subscribeRoomState() {
        liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .playing, .pushing:
                        self.topView.isHidden = false
                        self.bottomMenu.isHidden = false
                        self.seatListView.isHidden = false
                    case .none, .finished, .previewing:
                        self.topView.isHidden = true
                        self.bottomMenu.isHidden = true
                        self.seatListView.isHidden = true
                }
                if status == .finished {
                    showAudienceEndView()
                }
            }
            .store(in: &cancellableSet)
        
        roomIdPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] roomId in
                guard let self = self else { return }
                self.barrageButton.setRoomId(roomId: roomId)
                self.barrageDisplayView.setRoomId(roomId: roomId)
                self.giftListView.setRoomId(roomId: roomId)
                self.giftDisplayView.setRoomId(roomId: roomId)
            }
            .store(in: &cancellableSet)
        
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageButton.setOwnerId(ownerId: ownerInfo.userId)
                self.barrageDisplayView.setOwnerId(ownerId: ownerInfo.userId)
            }
            .store(in: &cancellableSet)
    }

    private func subscribeTopViewState() {
        coverUrl
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] url in
                guard let self = self else { return }
                self.backgroundImageView.kf.setImage(with: url)
            })
            .store(in: &cancellableSet)
        userCount
            .receive(on: RunLoop.main)
            .assign(to: \TopView.memberCount, on: topView)
            .store(in: &cancellableSet)
        memberAvatars
            .receive(on: RunLoop.main)
            .assign(to: \TopView.memberAvatars, on: topView)
            .store(in: &cancellableSet)
    }

    private func subscribeSeatState() {
        seatCount
            .receive(on: RunLoop.main)
            .assign(to: \SeatListView.seatCount, on: seatListView)
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
}

extension VoiceRoomRootView: TopViewDelegate {
    func topView(_ topView: TopView, tap event: TopView.TapEvent, sender: Any?) {
        switch event {
        case .stop:
            if store.selectCurrent(UserSelectors.isOwner) {
                let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
                designConfig.backgroundColor = .g2
                designConfig.lineColor = .g3.withAlphaComponent(0.1)
                let item = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
                    guard let self = self else { return }
                    self.showAnchorEndView()
                    self.musicPanelView.resetMusicPanelState()
                })
                viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .listMenu([item])))
            } else {
                store.dispatch(action: RoomActions.leave())
            }
        case .roomInfo:
            viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .roomInfoPanel))
        case .audienceList:
            viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .audienceList))
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
            .sink(receiveValue: { [weak seatView] seatInfo in
                seatView?.seatInfo = seatInfo
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
        let menus = BottomMenuViewDataHelper().generateOperateSeatMenuData(seat: seatInfo)
        if menus.count > 0 {
            viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .listMenu(menus)))
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
        view.setBalance(1_000)
    }

    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int) {
        let receiver = TUIGiftUser()
        receiver.userId = store.selectCurrent(RoomSelectors.roomOwnerId)
        receiver.userName = ""
        receiver.avatarUrl = ""
        receiver.level = "0"
        view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
    }
}

extension VoiceRoomRootView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let userId = store.selectCurrent(UserSelectors.currentUserId)
        let isOwner = store.selectCurrent(UserSelectors.isOwner)
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
        giftCacheService.request(urlString: gift.animationUrl) { error, data in
            guard let data = data else { return }
            if error == 0 {
                DispatchQueue.main.async {
                    giftPlayView.playGiftAnimation(animationData: data)
                }
            }
        }
    }
}

fileprivate extension String {
    static let meText = localized("live.barrage.me")
    static var confirmCloseText = localized("live.anchor.confirm.close")
    
}
