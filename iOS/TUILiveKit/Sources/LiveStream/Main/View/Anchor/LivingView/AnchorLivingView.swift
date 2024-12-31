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
import LiveStreamCore

class AnchorLivingView: UIView {
    
    private let roomId: String
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private let coreView: LiveCoreView
    
    private var cancellableSet: Set<AnyCancellable> = []
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    
    private let liveInfoView: LiveInfoView = {
        let view = LiveInfoView()
        view.mm_h = 32.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()
    
    private lazy var closeButton: UIButton = {
        let view = UIButton(frame: .zero)
        view.setImage(.liveBundleImage("live_end_live_icon"), for: .normal)
        view.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return view
    }()
    
    private let audienceListView: AudienceListView = {
        let view = AudienceListView()
        return view
    }()
    
    private lazy var bottomMenu: LSBottomMenuView = {
        let view = LSBottomMenuView(mananger: manager, routerManager: routerManager, coreView: coreView, isOwner: true)
        return view
    }()
    
    private lazy var floatView: LinkMicAnchorFloatView = {
        let view = LinkMicAnchorFloatView(manager: manager, routerManager: routerManager)
        view.isHidden = true
        return view
    }()
    
    private lazy var barrageDisplayView: BarrageStreamView = {
        let ownerId = manager.roomState.ownerInfo.userId
        let view = BarrageStreamView(roomId: roomId)
        view.delegate = self
        return view
    }()
    
    private lazy var giftDisplayView: GiftPlayView = {
        let view = GiftPlayView(roomId: roomId)
        view.delegate = self
        return view
    }()
    
    private lazy var barrageSendView: BarrageInputView = {
        var view = BarrageInputView(roomId: roomId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        view.backgroundColor = .g1.withAlphaComponent(0.4)
        view.delegate = self
        return view
    }()
    
    init(roomId: String, manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
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
        isViewReady = true
    }
    
    private func bindInteraction() {
        subscribeState()
    }
    
    private func subscribeState() {
        manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.requestCoGuestList))
            .receive(on: RunLoop.main)
            .sink { [weak self] seatApplicationList in
                guard let self = self else { return }
                self.showLinkMicFloatView(isPresent: seatApplicationList.count > 0)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeRoomState(StateSelector(keyPath: \LSRoomState.ownerInfo))
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageDisplayView.setOwnerId(ownerInfo.userId)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeRoomState(StateSelector(keyPath: \LSRoomState.liveStatus))
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .pushing:
                        self.didEnterRoom()
                        self.initComponentView()
                    default:
                        break
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func didEnterRoom() {
        TUICore.notifyEvent(TUICore_PrivacyService_ROOM_STATE_EVENT_CHANGED,
                            subKey: TUICore_PrivacyService_ROOM_STATE_EVENT_SUB_KEY_START,
                            object: nil,
                            param: nil)
    }
    
    func initComponentView() {
        initAudienceListView()
        initLiveInfoView()
    }
    
    func initAudienceListView() {
        audienceListView.initialize(roomId: roomId)
    }
    
    func initLiveInfoView() {
        liveInfoView.initialize(roomId: roomId)
    }
}

// MARK: Layout

extension AnchorLivingView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(closeButton)
        addSubview(audienceListView)
        addSubview(liveInfoView)
        addSubview(bottomMenu)
        addSubview(floatView)
        addSubview(barrageSendView)
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
    
    func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        barrageDisplayView.snp.remakeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.width.equalTo(305.scale375())
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
        
        liveInfoView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(liveInfoView.mm_h)
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
        
        let selfUserId = manager.userState.selfInfo.userId
        let isSelfInConnection = manager.coHostState.connectedUsers.count > 1
        let isSelfInBattle = manager.battleState.battleUsers.contains(where: { $0.userId == selfUserId }) && isSelfInConnection
        
        if isSelfInBattle {
            title = .endLiveOnBattleText
            let endBattleItem = ActionItem(title: .endLiveBattleText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                exitBattle()
                self.routerManager.router(action: .dismiss())
            })
            items.append(endBattleItem)
        } else if isSelfInConnection {
            title = .endLiveOnConnectionText
            let endConnectionItem = ActionItem(title: .endLiveDisconnectText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                coreView.terminateCrossRoomConnection()
                manager.coHostManager.update(connectedUser: [])
                self.routerManager.router(action: .dismiss())
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
            self.routerManager.router(action: .dismiss())
        })
        items.append(endLiveItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(title: title, items: items))))
    }
    
    private func exitBattle() {
        coreView.terminateBattle(battleId: manager.battleState.battleId) {
        } onError: { _, _ in
        }
    }
    
    private func showEndView() {
        coreView.stopLiveStream() { [weak self] in
            guard let self = self else { return }
            manager.update(liveStatus: .finished)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
        }
        let roomState = manager.roomState
        let giftIncome = roomState.liveExtraInfo.giftIncome
        let giftPeopleCount = roomState.liveExtraInfo.giftPeopleSet.count
        let audienceCount = manager.roomState.userCount
        let liveDataModel = LiveDataModel(roomId: roomId,
                                          liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                          audienceCount: audienceCount == 0 ? 0 : audienceCount,
                                          messageCount: barrageDisplayView.getBarrageCount(),
                                          giftIncome: giftIncome,
                                          giftPeopleCount: giftPeopleCount,
                                          likeCount: giftDisplayView.getLikeCount())
        let anchorEndView = AnchorEndView(liveDataModel: liveDataModel)
        anchorEndView.delegate = self
        addSubview(anchorEndView)
        anchorEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func presentPopup(view: UIView) {
        if let vc = WindowUtils.getCurrentWindowViewController() {
            let menuContainerView = LSMenuContainerView(contentView: view)
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

extension AnchorLivingView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell(barrage: barrage)
    }
}


extension AnchorLivingView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let selfUserId = manager.userState.selfInfo.userId
        if selfUserId == receiver.userId {
            manager.update { state in
                state.liveExtraInfo.giftIncome += gift.price * giftCount
                state.liveExtraInfo.giftPeopleSet.insert(sender.userId)
            }
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

extension AnchorLivingView: LiveEndViewDelegate {
    func onCloseButtonClick() {
        routerManager.router(action: .exit)
    }
}

extension AnchorLivingView: BarrageInputViewDelegate {
    func barrageInputViewOnSendBarrage(_ barrage: TUIBarrage) {
        barrageDisplayView.insertBarrages([barrage])
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
