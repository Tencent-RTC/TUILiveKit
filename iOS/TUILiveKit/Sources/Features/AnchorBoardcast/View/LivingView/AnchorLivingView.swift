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
import RTCRoomEngine

class AnchorLivingView: UIView {
    
    private let roomId: String
    private let manager: AnchorManager
    private let routerManager: AnchorRouterManager
    private let coreView: LiveCoreView
    private let netWorkInfoManager = NetWorkInfoManager(
        service: NetWorkInfoService(
            trtcCloud: TUIRoomEngine.sharedInstance().getTRTCCloud()
        )
    )
    private var cancellableSet: Set<AnyCancellable> = []
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    
    private let liveInfoView: LiveInfoView = {
        let view = LiveInfoView(enableFollow: VideoLiveKit.createInstance().enableFollow)
        view.mm_h = 40.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()
    
    private lazy var closeButton: UIButton = {
        let view = UIButton(frame: .zero)
        view.setImage(internalImage("live_end_live_icon"), for: .normal)
        view.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        view.imageEdgeInsets = UIEdgeInsets(top: 2.scale375(), left: 2.scale375(), bottom: 2.scale375(), right: 2.scale375())
        return view
    }()
    
    private lazy var audienceListView: AudienceListView = {
        let view = AudienceListView()
        view.onUserManageButtonClicked = { [weak self] userId in
            guard let self = self else { return }
            routerManager.router(action: .present(.userManagement(userId, type: .messageAndKickOut)))
        }
        return view
    }()
    
    private lazy var bottomMenu: AnchorBottomMenuView = {
        let view = AnchorBottomMenuView(mananger: manager, routerManager: routerManager, coreView: coreView)
        return view
    }()
    
    private lazy var floatView: LinkMicAnchorFloatView = {
        let view = LinkMicAnchorFloatView(manager: manager, routerManager: routerManager)
        view.isHidden = true
        return view
    }()
    
    private lazy var barrageDisplayView: BarrageStreamView = {
        let ownerId = manager.coreRoomState.ownerInfo.userId
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
        view.layer.cornerRadius = 20.scale375Height()
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var floatWindowButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(internalImage("live_floatwindow_open_icon"), for: .normal)
        button.addTarget(self, action: #selector(onFloatWindowButtonClick), for: .touchUpInside)
        button.imageEdgeInsets = UIEdgeInsets(top: 2.scale375(), left: 2.scale375(), bottom: 2.scale375(), right: 2.scale375())
        return button
    }()
    
    private lazy var netWorkInfoButtom: NetworkInfoButton = {
        let button = NetworkInfoButton(manager: netWorkInfoManager)
        button.onNetWorkInfoButtonClicked = { [weak self] in
            guard let self = self else { return }
            routerManager.router(action: .present(.netWorkInfo(netWorkInfoManager,isAudience: false)))
        }
        return button
    }()

    private lazy var netWorkStatusToastView: NetworkStatusToastView = {
        let view = NetworkStatusToastView()
        view.onCloseButtonTapped = { [weak self] in
            guard let self = self else { return }
            netWorkStatusToastView.isHidden = true
            self.netWorkInfoManager.onNetWorkInfoStatusToastViewClosed()
        }
        view.isHidden = true
        return view
    }()
    
    private var anchorObserverState = ObservableState<AnchorState>(initialState: AnchorState())
    
    init(roomId: String, manager: AnchorManager, routerManager: AnchorRouterManager, coreView: LiveCoreView) {
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
        if manager.roomState.liveStatus == .pushing {
            coreView.stopLiveStream() {
            } onError: { _, _ in
            }
        }
        print("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    private func bindInteraction() {
        subscribeState()
        subscribeSubject()
    }
    
    private func subscribeState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.applicantList))
            .receive(on: RunLoop.main)
            .sink { [weak self] seatApplicationList in
                guard let self = self else { return }
                if manager.coreCoHostState.receivedConnectionRequest != nil || manager.coreCoHostState.sentConnectionRequestList.count > 0 {
                    // If received connection request first, reject all linkmic auto.
                    for seatApplication in seatApplicationList {
                        coreView.respondIntraRoomConnection(userId: seatApplication.userId, isAccepted: false) {} onError: { _, _ in }
                    }
                    return
                }
                self.showLinkMicFloatView(isPresent: seatApplicationList.count > 0)
            }
            .store(in: &cancellableSet)
        manager.subscribeCoreViewState(StateSelector(keyPath: \RoomState.ownerInfo))
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageDisplayView.setOwnerId(ownerInfo.userId)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \AnchorRoomState.liveStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
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

        manager.subscribeState(StateSelector(keyPath: \AnchorRoomState.createTime))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .dropFirst()
            .sink { [weak self] createTime in
                self?.netWorkInfoButtom.onRecivedCreateTime(timer: TimeInterval(createTime))
            }
            .store(in: &cancellableSet)

        netWorkInfoManager
            .subscribe(StateSelector(keyPath: \NetWorkInfoState.showToast))
            .receive(on: RunLoop.main)
            .sink { [weak self] showToast in
                guard let self = self else { return }
                if showToast {
                    self.netWorkStatusToastView.isHidden = false
                } else {
                    self.netWorkStatusToastView.isHidden = true
                }
            }
            .store(in: &cancellableSet)

    }
    
    private func subscribeSubject() {
        manager.kickedOutSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] isDismissed in
                guard let self = self else { return }
                routerManager.router(action: .dismiss())
                if isDismissed {
                    makeToast(.roomDismissText)
                } else {
                    makeToast(.kickedOutText)
                }
                DispatchQueue.main.asyncAfter(deadline: .now() + 2) { [weak self] in
                    guard let self = self else { return }
                    routerManager.router(action: .exit)
                }
            }.store(in: &cancellableSet)
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
        audienceListView.initialize(roomInfo: manager.roomState.roomInfo)
    }
    
    func initLiveInfoView() {
        liveInfoView.initialize(roomInfo: manager.roomState.roomInfo)
    }
    
    @objc func onFloatWindowButtonClick() {
        manager.floatWindowSubject.send()
    }
    
    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        let view = super.hitTest(point, with: event)
        return view == self ? nil : view
    }
}

extension AnchorLivingView {
    func disableFeature(_ feature: AnchorViewFeature, isDisable: Bool) {
        switch feature {
        case .liveData:
            liveInfoView.isHidden = isDisable
        case .visitorCnt:
            audienceListView.isHidden = isDisable
        case .coGuest:
            bottomMenu.disableFeature(.coGuest, isDisable: isDisable)
        case .coHost:
            bottomMenu.disableFeature(.coHost, isDisable: isDisable)
        case .battle:
            bottomMenu.disableFeature(.battle, isDisable: isDisable)
        case .soundEffect:
            bottomMenu.disableFeature(.soundEffect, isDisable: isDisable)
        }
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
        addSubview(floatWindowButton)
        addSubview(netWorkInfoButtom)
        addSubview(netWorkStatusToastView)
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
            make.left.equalToSuperview().offset(12.scale375())
            make.width.equalTo(305.scale375())
            make.height.equalTo(212.scale375Height())
            make.bottom.equalTo(barrageSendView.snp.top).offset(-16.scale375Height())
        }
        
        closeButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.trailing.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
            make.top.equalToSuperview().inset((self.isPortrait ? 70 : 24).scale375Height())
        }
        
        floatWindowButton.snp.makeConstraints { make in
            make.trailing.equalTo(closeButton.snp.leading).offset(-8.scale375())
            make.centerY.equalTo(closeButton)
            make.width.equalTo(24.scale375Width())
            make.height.equalTo(24.scale375Width())
        }
        
        audienceListView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.trailing.equalTo(floatWindowButton.snp.leading).offset(-8.scale375())
            make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
        }
        
        liveInfoView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(liveInfoView.mm_h)
            make.leading.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
            make.width.lessThanOrEqualTo(160.scale375())
        }
        
        bottomMenu.snp.makeConstraints { make in
            make.bottom.equalToSuperview().offset(-38.scale375Height())
            make.trailing.equalToSuperview()
            make.height.equalTo(46.scale375Height())
        }
        
        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375())
            make.height.equalTo(86.scale375())
            make.width.equalTo(114.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
        
        barrageSendView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(12.scale375())
            make.width.equalTo(120.scale375())
            make.height.equalTo(40.scale375Height())
            make.centerY.equalTo(bottomMenu)
        }

        netWorkInfoButtom.snp.makeConstraints { make in
            make.top.equalTo(floatWindowButton.snp.bottom).offset(10.scale375())
            make.height.equalTo(20.scale375())
            make.width.equalTo(74.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }

        netWorkStatusToastView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(386.scale375())
            make.width.equalTo(262.scale375())
            make.height.equalTo(40.scale375())
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
        
        let selfUserId = manager.coreUserState.selfInfo.userId
        let isSelfInCoGuestConnection = manager.coreCoGuestState.connectedUserList.count > 1
        let isSelfInCoHostConnection = manager.coHostState.connectedUsers.count > 1
        let isSelfInBattle = manager.battleState.battleUsers.contains(where: { $0.userId == selfUserId }) && isSelfInCoHostConnection
        
        if isSelfInBattle {
            title = .endLiveOnBattleText
            let endBattleItem = ActionItem(title: .endLiveBattleText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                exitBattle()
                self.routerManager.router(action: .dismiss())
            })
            items.append(endBattleItem)
        } else if isSelfInCoHostConnection {
            title = .endLiveOnConnectionText
            let endConnectionItem = ActionItem(title: .endLiveDisconnectText, designConfig: lineConfig, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                coreView.terminateCrossRoomConnection()
                manager.onCrossRoomConnectionTerminated()
                self.routerManager.router(action: .dismiss())
            })
            items.append(endConnectionItem)
        } else if isSelfInCoGuestConnection {
            title = .endLiveOnLinkMicText
        }
        
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endLiveItem = ActionItem(title: .confirmCloseText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.exitBattle()
            self.stopLiveStream()
            self.routerManager.router(action: .dismiss())
        })
        items.append(endLiveItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(title: title, items: items, cancelText: .cancelText))))
    }
    
    private func exitBattle() {
        coreView.terminateBattle(battleId: manager.battleState.battleId) {
        } onError: { _, _ in
        }
    }
    
    private func fetchRoomStatistics(completion: @escaping () -> Void) {
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
    
    func stopLiveStream() {
        fetchRoomStatistics { [weak self] in
            guard let self = self else { return }
            coreView.stopLiveStream() { [weak self] in
                guard let self = self else { return }
                manager.onStopLive()
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(code: code.rawValue, message: message)
                manager.onError(error)
            }
            showEndView()
        }
    }

    func showEndView() {
        anchorObserverState.update { [weak self] state in
            guard let self = self else { return }
            state.duration = abs(Int(Date().timeIntervalSince1970 - Double(manager.roomState.createTime / 1_000)))
            state.viewCount = manager.roomState.liveExtraInfo.maxAudienceCount
            state.giftTotalCoins = manager.roomState.liveExtraInfo.giftTotalCoins
            state.giftTotalUniqueSender = manager.roomState.liveExtraInfo.giftTotalUniqueSender
            state.likeTotalUniqueSender = manager.roomState.liveExtraInfo.likeTotalUniqueSender
            state.messageCount = barrageDisplayView.getBarrageCount()
            manager.onEndLivingSubject.send(state)
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
}

extension AnchorLivingView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return GiftBarrageCell(barrage: barrage)
    }

    func onBarrageClicked(user: TUIUserInfo) {
        if user.userId == manager.coreUserState.selfInfo.userId { return }
        routerManager.router(action: .present(.userManagement(user, type: .messageAndKickOut)))
    }
}

extension AnchorLivingView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: TUIGiftInfo, giftCount: Int, sender: TUIUserInfo) {
        let barrage = TUIBarrage()
        barrage.content = "gift"
        barrage.user.userId = sender.userId
        barrage.user.userName = sender.userName
        barrage.user.avatarUrl = sender.avatarUrl
        barrage.user.level = "0"
        barrage.extInfo["TYPE"] = AnyCodable("GIFTMESSAGE")
        barrage.extInfo["gift_name"] = AnyCodable(gift.name)
        barrage.extInfo["gift_count"] = AnyCodable(giftCount)
        barrage.extInfo["gift_icon_url"] = AnyCodable(gift.iconUrl)
        
        let receiver = manager.coreRoomState.ownerInfo
        if receiver.userId == TUILogin.getUserID() {
            receiver.userName = .meText
        }
        barrage.extInfo["gift_receiver_username"] = AnyCodable(receiver.userName)
        barrageDisplayView.insertBarrages([barrage])
    }
    
    func giftPlayView(_ giftPlayView: GiftPlayView, onPlayGiftAnimation gift: TUIGiftInfo) {
        guard let url = URL(string: gift.resourceUrl) else { return }
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
    static let confirmCloseText = internalLocalized("End Live")
    static let meText = internalLocalized("Me")
    
    static let endLiveOnConnectionText = internalLocalized("You are currently co-hosting with other streamers. Would you like to [End Co-host] or [End Live] ?")
    static let endLiveDisconnectText = internalLocalized("End Co-host")
    static let endLiveOnLinkMicText = internalLocalized("You are currently co-guesting with other streamers. Would you like to [End Live] ?")
    static let endLiveOnBattleText = internalLocalized("You are currently in PK mode. Would you like to [End PK] or [End Live] ?")
    static let endLiveBattleText = internalLocalized("End PK")
    static let cancelText = internalLocalized("Cancel")
    static let kickedOutText = internalLocalized("You have been kicked out of the room")
    static let roomDismissText = internalLocalized("Broadcast has been ended")
}
