//
//  AudienceLivingView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/15.
//

import TUICore
import UIKit
import RTCCommon
import Combine
import LiveStreamCore
import RTCRoomEngine
import TUIGift
import TUIBarrage
import TUILiveInfo
import TUIAudienceList

class AudienceLivingView: RTCBaseView {
    // MARK: - private property.
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private let coreView: LiveCoreView
    
    private var cancellableSet = Set<AnyCancellable>()
    private let giftCacheService = TUIGiftStore.shared.giftCacheService
    private lazy var ownerInfoPublisher  = manager.subscribeCoreViewState(StateSelector(keyPath: \RoomState.ownerInfo))
    
    private let liveInfoView: LiveInfoView = {
        let view = LiveInfoView(enableFollow: VideoLiveKit.createInstance().enableFollow)
        view.mm_h = 32.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()

    private let audienceListView: AudienceListView = {
        var view = AudienceListView()
        return view
    }()

    private lazy var reportBtn: UIButton  = {
        let btn = UIButton(type: .custom)
        btn.setImage(.liveBundleImage("live_report"), for: .normal)
        btn.imageView?.contentMode = .scaleAspectFill
        btn.addTarget(self, action: #selector(clickReport), for: .touchUpInside)
        return btn
    }()
    
    private lazy var floatWindowButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(.liveBundleImage("live_floatwindow_open_icon"), for: .normal)
        button.addTarget(self, action: #selector(onFloatWindowButtonClick), for: .touchUpInside)
        return button
    }()
    
    private let leaveButton: UIButton = {
        let button = UIButton()
        button.setImage(.liveBundleImage("live_leave_icon"), for: .normal)
        return button
    }()

    private lazy var barrageSendView: BarrageInputView = {
        let roomId = manager.roomState.roomId
        var view = BarrageInputView(roomId: roomId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        view.layer.masksToBounds = true
        return view
    }()

    private lazy var bottomMenu: LSBottomMenuView = {
        let view = LSBottomMenuView(mananger: manager, routerManager: routerManager, coreView: coreView, isOwner: false)
        return view
    }()

    private lazy var floatView: LinkMicAudienceFloatView = {
        let view = LinkMicAudienceFloatView(manager: manager, routerManager: routerManager)
        view.delegate = self
        view.isHidden = true
        return view
    }()

    private lazy var barrageDisplayView: BarrageStreamView = {
        let roomId = manager.roomState.roomId
        let ownerId = manager.coreRoomState.ownerInfo.userId
        let view = BarrageStreamView(roomId: roomId)
        view.delegate = self
        return view
    }()

    private lazy var giftDisplayView: GiftPlayView = {
        let view = GiftPlayView(roomId: manager.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    init(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
    }

    override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(liveInfoView)
        addSubview(audienceListView)
#if RTCube_APPSTORE
        addSubview(reportBtn)
#endif
        addSubview(floatWindowButton)
        addSubview(leaveButton)
        addSubview(bottomMenu)
        addSubview(floatView)
        addSubview(barrageSendView)
    }

    override func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }

        barrageDisplayView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.bottom.equalTo(barrageSendView.snp.top).offset(-20.scale375Height())
            make.width.equalTo(305.scale375Width())
            make.height.equalTo(212.scale375Height())
        }

        liveInfoView.snp.remakeConstraints { make in
            make.top.equalToSuperview().offset(54.scale375Height())
            make.height.equalTo(liveInfoView.frame.height)
            make.leading.equalToSuperview().inset(16.scale375())
        }

        leaveButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-20.scale375Width())
            make.top.equalToSuperview().offset(58.scale375Height())
            make.width.equalTo(24.scale375Width())
            make.height.equalTo(24.scale375Width())
        }
        
        floatWindowButton.snp.makeConstraints { make in
            make.trailing.equalTo(leaveButton.snp.leading).offset(-8.scale375Width())
            make.top.equalToSuperview().offset(61.scale375Height())
            make.width.equalTo(18.scale375Width())
            make.height.equalTo(18.scale375Width())
        }
        
#if RTCube_APPSTORE
        reportBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(floatWindowButton)
            make.right.equalTo(floatWindowButton.snp.left).offset(-8)
            make.width.height.equalTo(24.scale375Width())
        })
        audienceListView.snp.makeConstraints { make in
            make.trailing.equalTo(reportBtn.snp.leading).offset(-4.scale375Width())
            make.centerY.equalTo(floatWindowButton)
            make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
        }
#else
        audienceListView.snp.makeConstraints { make in
            make.trailing
                .equalTo(floatWindowButton.snp.leading)
                .offset(-4.scale375Width())
            make.centerY.equalTo(floatWindowButton)
            make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
        }
#endif

        barrageSendView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375())
            make.width.equalTo(130.scale375())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }

        bottomMenu.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-16.scale375Width())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }
        
        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
            make.height.width.equalTo(86.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
    }
    override func bindInteraction() {
        subscribeSeatSubject()
        leaveButton.addTarget(self, action: #selector(leaveButtonClick), for: .touchUpInside)
    }
    
    func initComponentView() {
        initAudienceListView()
        initLiveInfoView()
    }
    
    func initAudienceListView() {
        audienceListView.initialize(roomId: manager.roomState.roomId)
    }
    
    func initLiveInfoView() {
        liveInfoView.initialize(roomId: manager.roomState.roomId)
    }
}

extension AudienceLivingView {

    private func subscribeSeatSubject() {
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageDisplayView.setOwnerId(ownerInfo.userId)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - View Action.
extension AudienceLivingView {
    
    @objc
    private func clickReport() {
        let selector = NSSelectorFromString("showReportAlertWithRoomId:ownerId:")
        if responds(to: selector) {
            perform(selector, with: manager.roomState.roomId, with: manager.coreRoomState.ownerInfo.userId)
        }
    }
    
    @objc func onFloatWindowButtonClick() {
        manager.floatWindowSubject.send()
    }

    @objc func leaveButtonClick() {
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
            coreView.terminateIntraRoomConnection()
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
        routerManager.router(action: .present(.listMenu(ActionPanelData(title: title, items: items))))
    }
    
    func leaveRoom() {
        coreView.leaveLiveStream() { [weak self] in
            guard let self = self else { return }
            manager.onLeaveLive()
        } onError: { _, _ in
        }
        routerManager.router(action: .exit)
    }
    
    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        let view = super.hitTest(point, with: event)
        return view == self ? nil : view
    }
}

extension AudienceLivingView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell(barrage: barrage)
    }
    
    func onBarrageClicked(user: TUIUserInfo) {
        if user.userId == manager.coreUserState.selfInfo.userId { return }
        routerManager.router(action: .present(.userManagement(user, type: .userInfo)))
    }
}

extension AudienceLivingView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
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

extension AudienceLivingView: LinkMicAudienceFloatViewDelegate {
    func cancelApplication() {
        manager.onStartCancelIntraRoomConnection()
        coreView.cancelIntraRoomConnection(userId: "") { [weak self] in
            guard let self = self else { return }
            manager.onCancelIntraRoomConnection()
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            manager.onCancelIntraRoomConnection()
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
        }
    }
}

private extension String {
    static let meText = localized("Me")
    static let endLiveOnLinkMicText = localized("You are currently co-guesting with other streamers. Would you like to [End Co-guest] or [Exit Live] ?")
    static let endLiveLinkMicDisconnectText = localized("End Co-guest")
    static let confirmCloseText = localized("Exit Live")
}
