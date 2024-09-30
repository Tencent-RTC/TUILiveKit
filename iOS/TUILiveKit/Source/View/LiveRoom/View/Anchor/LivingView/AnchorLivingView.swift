//
//  AnchorLivingView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/19.
//

import Foundation
import TUICore
import RTCCommon
import Combine

class AnchorLivingView: UIView {
    private let roomId:String
    private let routerStore: RouterStore
    private let store: LiveStoreProvider
    private let viewStore: LiveRoomViewStore
    
    private lazy var ownerInfoPublisher = store.select(RoomSelectors.getRoomOwnerInfo)
    private lazy var roomIdPublisher = store.select(RoomSelectors.getRoomId)
    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    
    private var cancellableSet: Set<AnyCancellable> = []
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    
    private lazy var topGradientView: UIView = {
        var view = UIView(frame: CGRect(x: 0, y: 0, width: self.mm_w, height: 142.scale375Height()))
        view.gradient(colors: [.g1.withAlphaComponent(0), .g1,], isVertical: true)
        return view
    }()
    
    private lazy var roomInfoView: RoomInfoView = {
        let view = RoomInfoView(store: store)
        view.mm_h = 32.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        view.addTarget(self, action: #selector(roomInfoViewClick), for: .touchUpInside)
        return view
    }()
    
    private lazy var closeButton: UIButton = {
        let view = UIButton(frame: .zero)
        view.setImage(.liveBundleImage("live_end_live_icon"), for: .normal)
        view.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return view
    }()
    
    private lazy var audienceListView: UIView = {
        let view = AudienceListView(store: store, routerStore: routerStore)
        return view
    }()
    
    private lazy var bottomMenu: BottomMenuView = {
        let view = BottomMenuView(frame: .zero)
        return view
    }()
    
    private lazy var floatView: LinkMicAnchorFloatView = {
        let view = LinkMicAnchorFloatView(store: store, routerStore: routerStore)
        view.isHidden = true
        return view
    }()
    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let ownerId = store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageDisplayView(roomId: roomId, ownerId: ownerId)
        view.delegate = self
        return view
    }()
    
    private lazy var giftDisplayView: TUIGiftPlayView = {
        let view = TUIGiftPlayView(groupId: roomId)
        view.delegate = self
        return view
    }()
    
    private lazy var barrageSendView: TUIBarrageButton = {
        let ownerId = store.selectCurrent(RoomSelectors.roomOwnerId)
        var view = TUIBarrageButton(roomId: roomId, ownerId: ownerId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        view.backgroundColor = .g1.withAlphaComponent(0.4)
        return view
    }()
    
    init(roomId: String, routerStore: RouterStore) {
        self.roomId = roomId
        self.store = LiveStoreFactory.getStore(roomId: roomId)
        self.routerStore = routerStore
        self.viewStore = LiveRoomViewStoreFactory.getStore(roomId: roomId)
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        topGradientView.gradient(colors: [.g1,
                                          .g1.withAlphaComponent(0),], isVertical: true)
        isViewReady = true
    }
    
    private func bindInteraction() {
        subscribeState()
    }
    
    private func subscribeState() {
        store.select(SeatSelectors.getSeatApplications)
            .receive(on: RunLoop.main)
            .sink { [weak self] seatApplicationList in
                guard let self = self else { return }
                self.showLinkMicFloatView(isPresent: seatApplicationList.count > 0)
            }
            .store(in: &cancellableSet)
        
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageSendView.setOwnerId(ownerId: ownerInfo.userId)
                self.barrageDisplayView.setOwnerId(ownerId: ownerInfo.userId)
            }
            .store(in: &cancellableSet)
        
        viewStore.select(LiveRoomViewSelectors.getBottomMenuButtons)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] menus in
                guard let self = self else { return }
                self.bottomMenu.menus = menus
            })
            .store(in: &cancellableSet)
        
        liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .pushing:
                        self.didEnterRoom()
                    default:
                        break
                }
            }
            .store(in: &cancellableSet)
    }
    
    
    
    private func didEnterRoom() {
        viewStore.dispatch(action: LiveRoomViewActions.updateBottomMenus(payload: (store, routerStore)))
    }
}

// MARK: Layout

extension AnchorLivingView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(topGradientView)
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(closeButton)
        addSubview(audienceListView)
        addSubview(roomInfoView)
        addSubview(bottomMenu)
        addSubview(floatView)
        addSubview(barrageSendView)
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
    
    func activateConstraints() {
        topGradientView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(142.scale375Height())
            make.width.equalToSuperview()
        }
        
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        barrageDisplayView.snp.remakeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.width.equalTo(268.scale375())
            make.height.equalTo(212.scale375Height())
            if self.isPortrait {
                make.bottom.equalTo(bottomMenu.snp.top).offset(-4.scale375Height())
            } else {
                make.bottom.equalTo(bottomMenu.snp.bottom)
            }
        }
        
        closeButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.trailing.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
            make.top.equalToSuperview().inset((self.isPortrait ? 58 : 24).scale375Height())
        }
        
        audienceListView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(24.scale375())
            make.width.equalTo(116.scale375())
            make.trailing.equalTo(closeButton.snp.leading).offset(-4.scale375())
        }
        
        roomInfoView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(roomInfoView.mm_h)
            make.width.greaterThanOrEqualTo(80.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
        }
        
        bottomMenu.snp.makeConstraints { make in
            make.bottom.equalToSuperview().offset(-34.scale375Height())
            make.trailing.equalToSuperview()
            make.height.equalTo(36)
        }
        
        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375())
            make.height.equalTo(86.scale375())
            make.width.equalTo(114.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
        
        barrageSendView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375())
            make.width.equalTo(130.scale375())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }
    }
}

// MARK: Action

extension AnchorLivingView {
    @objc
    func closeButtonClick() {
        var title: String = ""
        var items: [ActionItem] = []
        let lineConfig = ActionItemDesignConfig(lineWidth: 1, titleColor: .redColor)
        lineConfig.backgroundColor = .white
        lineConfig.lineColor = .g8
        
        let selfUserId = store.selectCurrent(UserSelectors.getSelfInfo).userId
        if store.selectCurrent(BattleSelectors.getBattleUsers).contains(where: {$0.userId == selfUserId}) {
            title = .endLiveOnBattleText
            let endBattleItem = ActionItem(title: .endLiveBattleText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                let battleId = store.selectCurrent(BattleSelectors.getBattleId)
                self.store.dispatch(action: BattleActions.exitBattle(payload: battleId))
                self.routerStore.router(action: .dismiss())
            })
            items.append(endBattleItem)
        } else if store.selectCurrent(ConnectionSelectors.isConnecting) {
            title = .endLiveOnConnectionText
            let endConnectionItem = ActionItem(title: .endLiveDisconnectText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                self.store.dispatch(action: ConnectionActions.disconnect())
                let selfUserId = store.selectCurrent(UserSelectors.getSelfInfo).userId
                if store.selectCurrent(BattleSelectors.getBattleUsers).contains(where: {$0.userId == selfUserId}) {
                    let battleId = store.selectCurrent(BattleSelectors.getBattleId)
                    self.store.dispatch(action: BattleActions.exitBattle(payload: battleId))
                }
                self.routerStore.router(action: .dismiss())
            })
            items.append(endConnectionItem)
        }
        
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endLiveItem = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.exitBattle()
            self.showEndView()
            self.routerStore.router(action: .dismiss())
        })
        items.append(endLiveItem)
        routerStore.router(action: .present(.listMenu(ActionPanelData(title: title, items: items))))
    }
    
    @objc
    func roomInfoViewClick() {
        routerStore.router(action: .present(.roomInfo))
    }
    
    private func exitBattle() {
        let battleId = store.selectCurrent(BattleSelectors.getBattleId)
        if !battleId.isEmpty {
            store.dispatch(action: BattleActions.exitBattle(payload: battleId))
        }
    }
    
    private func showEndView() {
        store.dispatch(action: RoomActions.stop(payload: ()))
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
    
    private func presentPopup(view: UIView) {
        if let vc = WindowUtils.getCurrentWindowViewController() {
            let menuContainerView = MenuContainerView(contentView: view)
            menuContainerView.blackAreaClickClosure = {
                vc.dismiss(animated: true)
            }
            let viewController = PopupViewController(contentView: menuContainerView)
            vc.present(viewController, animated: true)
        }
    }
}

extension AnchorLivingView {
    func showLinkMicFloatView(isPresent: Bool) {
        floatView.isHidden = !isPresent
    }
    
    func getBarrageCount() -> Int {
        barrageDisplayView.getBarrageCount()
    }
    
    func getLikeCount() -> Int {
        giftDisplayView.getLikeCount()
    }
    
}

extension AnchorLivingView: TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell.getCustomCell(barrage: barrage)
    }
}


extension AnchorLivingView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let selfUserId = store.selectCurrent(UserSelectors.currentUserId)
        if selfUserId == receiver.userId {
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
    static let confirmCloseText = localized("live.anchor.confirm.close")
    static let sendText = localized("live.giftView.sendOut")
    static let meText = localized("live.barrage.me")
    
    static let endLiveOnConnectionText = localized("live.endLive.onConnection.alert")
    static let endLiveDisconnectText = localized("live.endLive.onConnection.alert.disconnect")
    static let endLiveOnLinkMicText = localized("live.endLive.onLinkMic.alert")
    static let endLiveLinkMicDisconnectText = localized("live.endLive.onLinkMic.alert.disconnect")
    static let endLiveOnBattleText = localized("live.endLive.onBattle.alert")
    static let endLiveBattleText = localized("live.endLive.onBattle.alert.endBattle")
}
