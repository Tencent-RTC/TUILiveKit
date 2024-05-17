//
//  VoiceRoomRootView.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import UIKit
import Combine
import SnapKit
import Kingfisher
import TUICore

class VoiceRoomRootView: UIView {
    
    @Injected var store: VoiceRoomStoreProvider
    
    private lazy var enterRoomState = self.store.select(RoomSelectors.getEnterRoomState)
    
    // TopView Status
    lazy var roomName = self.store.select(RoomSelectors.getRoomName)
    lazy var memberCount = self.store.select(RoomSelectors.getMemberCount)
    lazy var coverUrl = self.store.select(RoomSelectors.getRoomCoverUrl)
    lazy var memberAvatars = self.store.select(UserSelectors.getMemberAvatars)
    
    // SeatList Status
    lazy var seatCount = self.store.select(SeatSelectors.getSeatCount)
    
    private var isViewReady: Bool = false
    private var cancellableSet = Set<AnyCancellable>()
    private let giftCacheService = GiftCacheService()
    
    private var isOwner: Bool {
       return store.selectCurrent(UserSelectors.isOwner)
    }
    
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
    
    private let bottomMenu: UIView = {
        let view = BottomMenuView(frame: .zero)
        return view
    }()
    
    private lazy var barrageButton: UIView = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let view = TUIBarrageButton(roomId: roomId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        return view
    }()
    
    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let roomId = self.store.selectCurrent(RoomSelectors.getRoomId)
        let view = TUIBarrageDisplayView(roomId: roomId)
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
        giftCloudServer.queryGiftInfoList { error, giftlist in
            if error == .noError {
                view.setGiftList(giftlist)
            }
        }
        view.setBalance(500)
        return view
    }()
    
    lazy var musicPanelView: MusicPanelView = {
        let view = MusicPanelView(frame: .zero)
        view.backButtonClickClosure = { [weak self] _ in
            guard let self = self else { return }
            self.store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
        }
        return view
    }()
    
    override func didMoveToWindow() {
        guard !isViewReady else { return }
        backgroundLayer.frame = bounds
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        setupViewStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        layer.insertSublayer(backgroundLayer, at: 0)
        addSubview(backgroundImageView)
        addSubview(giftDisplayView)
        addSubview(topView)
        addSubview(seatListView)
        addSubview(bottomMenu)
        addSubview(barrageDisplayView)
        addSubview(barrageButton)
    }
    
    private func activeViewConstraint() {
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
            make.leading.equalToSuperview().offset(16)
            make.centerY.equalTo(bottomMenu.snp.centerY)
            make.height.equalTo(36)
            make.trailing.equalTo(bottomMenu.snp.leading).offset(-12)
        }
        giftDisplayView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
    }
    
    private func bindInteraction() {
        // Top view interaction.
        topView.delegate = self
        subscribeRoomState()
        subscribeTopViewState()
        seatListView.delegate = self
        subscribeSeatState()
        subscribeCustomEvent()
    }
    
    private func setupViewStyle() {}
    
    private func showEndView() {
        if store.selectCurrent(UserSelectors.isOwner) {
            let roomState = store.selectCurrent(RoomSelectors.getRoomState)
            let giftPrice = store.selectCurrent(UserSelectors.getReceivedGiftTotalPrice)
            let giftPeopleCount = store.selectCurrent(UserSelectors.getSendGiftUsers).count
            let audienceCount = store.selectCurrent(UserSelectors.getAudienceList).count
            let liveDataModel = LiveDataModel(liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                              audienceCount: audienceCount,
                                              messageCount: barrageDisplayView.getBarrageCount(),
                                              giftIncome: giftPrice,
                                              giftPeopleCount: giftPeopleCount,
                                              likeCount: giftDisplayView.getLikeCount())
            let anchorEndView = AnchorEndView(liveDataModel: liveDataModel)
            addSubview(anchorEndView)
            anchorEndView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        } else {
            guard let roomOwner = store.selectCurrent(UserSelectors.getRoomOwnerInfo) else { return }
            let audienceEndView = AudienceEndView(avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
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
        enterRoomState
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                switch state {
                    case .inRoom:
                        self.topView.isHidden = false
                        self.bottomMenu.isHidden = false
                        self.seatListView.isHidden = false
                    case .notEntered:
                        self.topView.isHidden = true
                        self.bottomMenu.isHidden = true
                        self.seatListView.isHidden = true
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeTopViewState() {
        roomName
            .receive(on: RunLoop.main)
            .assign(to: \TopView.roomInfoView.roomName, on: topView)
            .store(in: &cancellableSet)
        coverUrl
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] url in
                guard let self = self else { return }
                self.topView.roomInfoView.roomCoverUrl = url
                self.backgroundImageView.kf.setImage(with: url)
            })
            .store(in: &cancellableSet)
        memberCount
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
        self.store.customEventSubject
            .receive(on: DispatchQueue.main)
            .sink { [weak self] action in
                guard let self = self, let action = action as? IdentifiableAction else { return }
                if  action.id.contains(ViewActions.like.id) {
                    self.giftListView.sendLike()
                } else if action.id.contains(ViewActions.endView.id) {
                    showEndView()
                }
            }
            .store(in: &cancellableSet)
    }
}

extension VoiceRoomRootView: TopViewDelegate {
    func topView(_ topView: TopView, tap event: TopView.TapEvent, sender: Any?) {
        switch event {
            case .stop:
                if store.selectCurrent(UserSelectors.isOwner) {
                    musicPanelView.resetMusicPanelState()
                    store.dispatch(action: RoomActions.stop())
                } else {
                    store.dispatch(action: RoomActions.leave())
                }
            case .audienceList:
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .audienceList))
            default:
                break
        }
    }
}

extension VoiceRoomRootView: SeatListViewDelegate {
    func seatListView(_ seatListView: SeatListView, needUpdateViewState seatView: SeatView, at indexPath: IndexPath) {
        // Here you can bind seat&user data for seat view.
        let seatInfoSelector = SeatSelectors.getSeatInfo(index: indexPath.item)
        let seatInfoPublisher = store.select(seatInfoSelector)
        let audioAvailableUsers = store.select(UserSelectors.getAudioAvailableUsers)
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
            .sink { [weak seatView, weak self] users, userId in
                guard let seatView = seatView, let self = self else { return }
                let seatInfo = self.store.selectCurrent(seatInfoSelector)
                if !seatInfo.userId.isEmpty {
                    seatView.isAudioMuted = !users.contains(seatInfo.userId)
                }
            }
            .store(in: &seatView.cancellableSet)
        let userVolumePublisher = store.select(UserSelectors.getSpeakingUsers)
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
        let menus = MenuDataCreator().generateOperateSeatMenuData(seat: seatInfo)
        if menus.count > 0 {
            store.dispatch(action: NavigatorActions.navigatorTo(payload: .listMenu(menus)))
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
        receiver.userId = self.store.selectCurrent(RoomSelectors.getRoomOwnerId)
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
            store.dispatch(action: UserActions.updateReceivedGiftTotalPrice(payload: gift.price * giftCount))
            store.dispatch(action: UserActions.updateSendGiftUser(payload: sender.userId))
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
}
