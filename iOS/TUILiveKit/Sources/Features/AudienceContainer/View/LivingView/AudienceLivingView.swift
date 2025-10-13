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
import AtomicXCore
import RTCRoomEngine

class AudienceLivingView: RTCBaseView {
    weak var rotateScreenDelegate: RotateScreenDelegate?
    
    // MARK: - private property.
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private let coreView: LiveCoreView
    private let netWorkInfoManager = NetWorkInfoManager(
        service: NetWorkInfoService(
            trtcCloud: TUIRoomEngine.sharedInstance().getTRTCCloud()
        )
    )
    private var cancellableSet = Set<AnyCancellable>()
    private let giftCacheService = GiftManager.shared.giftCacheService
    private lazy var ownerInfoPublisher  = manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \RoomState.ownerInfo))
    private let liveInfoView: LiveInfoView = {
        let view = LiveInfoView(enableFollow: VideoLiveKit.createInstance().enableFollow)
        view.mm_h = 40.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        view.isHidden = AudienceManager.audienceContainerConfig.disableHeaderLiveData
        return view
    }()

    private let audienceListView: AudienceListView = {
        var view = AudienceListView()
        view.isHidden = AudienceManager.audienceContainerConfig.disableHeaderVisitorCnt
        return view
    }()

    private lazy var reportBtn: UIButton  = {
        let btn = UIButton(type: .custom)
        btn.setImage(internalImage("live_report"), for: .normal)
        btn.imageView?.contentMode = .scaleAspectFill
        btn.addTarget(self, action: #selector(clickReport), for: .touchUpInside)
        return btn
    }()
    
    private lazy var floatWindowButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(internalImage("live_floatwindow_open_icon"), for: .normal)
        button.addTarget(self, action: #selector(onFloatWindowButtonClick), for: .touchUpInside)
        button.imageEdgeInsets = UIEdgeInsets(top: 2.scale375(), left: 2.scale375(), bottom: 2.scale375(), right: 2.scale375())
        button.isHidden = AudienceManager.audienceContainerConfig.disableHeaderFloatWin
        return button
    }()
    
    private lazy var rotateScreenButton: UIButton = {
         let button = UIButton()
         button.setImage(internalImage("live_rotate_screen"), for: .normal)
         button.addTarget(self, action: #selector(rotateScreenClick), for: .touchUpInside)
         button.imageEdgeInsets = UIEdgeInsets(top: 2.scale375(), left: 2.scale375(), bottom: 2.scale375(), right: 2.scale375())
         button.isHidden = true
         return button
     }()

    private lazy var barrageSendView: BarrageInputView = {
        let roomId = manager.roomState.roomId
        var view = BarrageInputView(roomId: roomId)
        view.layer.cornerRadius = 20.scale375Height()
        view.layer.masksToBounds = true
        return view
    }()

    private lazy var bottomMenu: AudienceBottomMenuView = {
        let view = AudienceBottomMenuView(mananger: manager, routerManager: routerManager, coreView: coreView, isOwner: false)
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
        let view = BarrageStreamView(liveId: roomId)
        view.delegate = self
        return view
    }()

    private lazy var giftDisplayView: GiftPlayView = {
        let view = GiftPlayView(roomId: manager.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    private lazy var netWorkInfoButton: NetworkInfoButton = {
        let button = NetworkInfoButton(liveId: manager.roomState.roomId, manager: netWorkInfoManager)
        button.onNetWorkInfoButtonClicked = { [weak self] in
            guard let self = self else { return }
            if !WindowUtils.isPortrait { return }
            let isOnSeat = manager.coGuestState.coGuestStatus == .linking
            routerManager.router(action: .present(AudienceRoute.netWorkInfo(netWorkInfoManager,isAudience: !isOnSeat)))
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

    private var playbackQuality: TUIVideoQuality? = nil
    
    init(manager: AudienceManager, routerManager: AudienceRouterManager, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
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
        addSubview(bottomMenu)
        addSubview(floatView)
        addSubview(barrageSendView)
        addSubview(netWorkInfoButton)
        addSubview(netWorkStatusToastView)
        addSubview(rotateScreenButton)
    }

    override func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }

        if WindowUtils.isPortrait {
            barrageDisplayView.snp.remakeConstraints { make in
                make.leading.equalToSuperview().offset(12.scale375())
                make.bottom.equalTo(barrageSendView.snp.top).offset(-12.scale375Height())
                make.width.equalTo(305.scale375Width())
                make.height.equalTo(212.scale375Height())
            }

            liveInfoView.snp.remakeConstraints { make in
                make.centerY.equalTo(floatWindowButton)
                make.height.equalTo(liveInfoView.frame.height)
                make.leading.equalToSuperview().inset(16.scale375())
                make.width.lessThanOrEqualTo(200.scale375())
            }
            
            floatWindowButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().offset(-48.scale375Width())
                make.top.equalToSuperview().offset(70.scale375Height())
                make.width.equalTo(24.scale375Width())
                make.height.equalTo(24.scale375Width())
            }
            
            rotateScreenButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().offset(-10.scale375Width())
                make.top.equalToSuperview().offset(475.scale375Height())
                make.width.equalTo(32.scale375Width())
                make.height.equalTo(32.scale375Width())
            }
            
    #if RTCube_APPSTORE
            reportBtn.snp.remakeConstraints({ make in
                make.centerY.equalTo(floatWindowButton)
                make.right.equalTo(floatWindowButton.snp.left).offset(-8)
                make.width.height.equalTo(24.scale375Width())
            })
            audienceListView.snp.remakeConstraints { make in
                make.trailing.equalTo(reportBtn.snp.leading).offset(-4.scale375Width())
                make.centerY.equalTo(floatWindowButton)
                make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
            }
    #else
            audienceListView.snp.remakeConstraints { make in
                make.trailing.equalToSuperview()
                    .offset(AudienceManager.audienceContainerConfig.disableHeaderFloatWin ? -48.scale375() : -80.scale375())
                make.centerY.equalTo(floatWindowButton)
                make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
            }
    #endif

            barrageSendView.snp.remakeConstraints { make in
                make.leading.equalToSuperview().offset(12.scale375())
                make.width.equalTo(124.scale375())
                make.height.equalTo(40.scale375Height())
                make.bottom.equalToSuperview().offset(-38.scale375Height())
            }

            bottomMenu.snp.remakeConstraints { make in
                make.trailing.equalToSuperview()
                make.height.equalTo(36.scale375Height())
                make.centerY.equalTo(barrageSendView)
            }
            
            floatView.snp.remakeConstraints { make in
                make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
                make.height.width.equalTo(86.scale375())
                make.trailing.equalToSuperview().offset(-8.scale375())
            }

            netWorkInfoButton.snp.remakeConstraints { make in
                make.top.equalTo(floatWindowButton.snp.bottom).offset(10.scale375())
                make.height.equalTo(20.scale375())
                make.width.equalTo(74.scale375())
                make.trailing.equalToSuperview().offset(-8.scale375())
            }

            netWorkStatusToastView.snp.remakeConstraints { make in
                make.centerX.equalToSuperview()
                make.top.equalToSuperview().offset(386.scale375())
                make.width.equalTo(262.scale375())
                make.height.equalTo(40.scale375())
            }
        } else {
            barrageDisplayView.snp.remakeConstraints { make in
                make.leading.equalToSuperview().offset(12.scale375())
                make.bottom.equalTo(barrageSendView.snp.top).offset(-12.scale375Height())
                make.width.equalTo(305.scale375Width())
                make.height.equalTo(212.scale375Height())
            }

            liveInfoView.snp.remakeConstraints { make in
                make.centerY.equalTo(floatWindowButton)
                make.height.equalTo(liveInfoView.frame.height)
                make.leading.equalToSuperview().inset(16.scale375())
                make.width.lessThanOrEqualTo(200.scale375())
            }
            
            floatWindowButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().offset(-48.scale375Width())
                make.top.equalToSuperview().offset(20.scale375Height())
                make.width.equalTo(24.scale375Width())
                make.height.equalTo(24.scale375Width())
            }
            
            rotateScreenButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().offset(-20.scale375Width())
                make.top.equalToSuperview().offset(185.scale375Height())
                make.width.equalTo(32.scale375Width())
                make.height.equalTo(32.scale375Width())
            }
            
    #if RTCube_APPSTORE
            reportBtn.snp.remakeConstraints({ make in
                make.centerY.equalTo(floatWindowButton)
                make.right.equalTo(floatWindowButton.snp.left).offset(-8)
                make.width.height.equalTo(24.scale375Width())
            })
            audienceListView.snp.remakeConstraints { make in
                make.trailing.equalTo(reportBtn.snp.leading).offset(-4.scale375Width())
                make.centerY.equalTo(floatWindowButton)
                make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
            }
    #else
            audienceListView.snp.remakeConstraints { make in
                make.trailing.equalToSuperview()
                    .offset(AudienceManager.audienceContainerConfig.disableHeaderFloatWin ? -48.scale375() : -80.scale375())
                make.centerY.equalTo(floatWindowButton)
                make.leading.greaterThanOrEqualTo(liveInfoView.snp.trailing).offset(20.scale375())
            }
    #endif

            barrageSendView.snp.remakeConstraints { make in
                make.leading.equalToSuperview().offset(12.scale375())
                make.width.equalTo(124.scale375())
                make.height.equalTo(40.scale375Height())
                make.bottom.equalToSuperview().offset(-38.scale375Height())
            }

            bottomMenu.snp.remakeConstraints { make in
                make.trailing.equalToSuperview()
                make.height.equalTo(36.scale375Height())
                make.centerY.equalTo(barrageSendView)
            }
            
            floatView.snp.remakeConstraints { make in
                make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
                make.height.width.equalTo(86.scale375())
                make.trailing.equalToSuperview().offset(-8.scale375())
            }

            netWorkInfoButton.snp.remakeConstraints { make in
                make.top.equalTo(floatWindowButton.snp.bottom).offset(10.scale375())
                make.height.equalTo(20.scale375())
                make.width.equalTo(74.scale375())
                make.trailing.equalToSuperview().offset(-8.scale375())
            }

            netWorkStatusToastView.snp.remakeConstraints { make in
                make.centerX.equalToSuperview()
                make.top.equalToSuperview().offset(386.scale375())
                make.width.equalTo(262.scale375())
                make.height.equalTo(40.scale375())
            }

        }
    }
    
    override func bindInteraction() {
        subscribeOrientationChange()
        subscribeRoomState()
        subscribeMediaState()
        subscribeSeatSubject()
        subscribeNetWorkInfoSubject()
        subscribeAudienceConfig()
    }
    
    func initComponentView() {
        initAudienceListView()
        initLiveInfoView()
    }
    
    func initAudienceListView() {
        audienceListView.initialize(liveId: manager.roomState.liveInfo.roomId)
    }
    
    func initLiveInfoView() {
        liveInfoView.initialize(liveInfo: manager.roomState.liveInfo)
    }
    
    func setGiftPureMode(_ isPureMode: Bool) {
        giftDisplayView.onPureModeSet(isPureMode: isPureMode)
    }
}

extension AudienceLivingView {
    private func subscribeOrientationChange() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(handleOrientationChange),
            name: Notification.Name.TUILiveKitRotateScreenNotification,
            object: nil
        )
    }

    private func subscribeRoomState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceRoomState.roomVideoStreamIsLandscape))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] videoStreamIsLandscape in
                guard let self = self else { return }
                if videoStreamIsLandscape {
                    self.rotateScreenButton.isHidden = false
                    self.bottomMenu.disableFooterCoGuest(true)
                } else {
                    self.rotateScreenButton.isHidden = true
                    if !AudienceManager.audienceContainerConfig.disableFooterCoGuest {
                        self.bottomMenu.disableFooterCoGuest(false)
                    }
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeMediaState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceMediaState.playbackQuality))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] playbackQuality in
                guard let self = self else {
                    return
                }
                if let quality = playbackQuality, self.playbackQuality != nil {
                    self.makeToast(.resolutionChangedText + .videoQualityToString(quality: quality))
                }
                self.playbackQuality = playbackQuality
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatSubject() {
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageDisplayView.setOwnerId(ownerInfo.userId)
            }
            .store(in: &cancellableSet)
    }

    private func subscribeNetWorkInfoSubject() {
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
    
    private func subscribeAudienceConfig() {
        AudienceManager.subscribeAudienceConfig(StateSelector(keyPath:
                                                                \AudienceContainerConfig.disableHeaderFloatWin))
        .receive(on: RunLoop.main)
        .removeDuplicates()
        .dropFirst()
        .sink { [weak self] disableHeaderFloatWin in
            guard let self = self else { return }
            floatWindowButton.isHidden = disableHeaderFloatWin
            audienceListView.snp.remakeConstraints { make in
                make.trailing
                    .equalToSuperview()
                    .offset(disableHeaderFloatWin ? -48.scale375() : -80.scale375())
                make.centerY.equalTo(self.floatWindowButton)
                make.leading.greaterThanOrEqualTo(self.liveInfoView.snp.trailing).offset(20.scale375())
            }
        }
        .store(in: &cancellableSet)
        
        AudienceManager.subscribeAudienceConfig(StateSelector(keyPath:
                                                                \AudienceContainerConfig.disableHeaderLiveData))
        .receive(on: RunLoop.main)
        .removeDuplicates()
        .dropFirst()
        .sink { [weak self] disableHeaderLiveData in
            guard let self = self else { return }
            liveInfoView.isHidden = disableHeaderLiveData
        }
        .store(in: &cancellableSet)
        
        AudienceManager.subscribeAudienceConfig(StateSelector(keyPath:
                                                                \AudienceContainerConfig.disableHeaderVisitorCnt))
        .receive(on: RunLoop.main)
        .removeDuplicates()
        .dropFirst()
        .sink { [weak self] disableHeaderVisitorCnt in
            guard let self = self else { return }
            audienceListView.isHidden = disableHeaderVisitorCnt
        }
        .store(in: &cancellableSet)
        
        AudienceManager.subscribeAudienceConfig(StateSelector(keyPath:
                                                                \AudienceContainerConfig.disableFooterCoGuest))
        .receive(on: RunLoop.main)
        .removeDuplicates()
        .dropFirst()
        .sink { [weak self] disableFooterCoGuest in
            guard let self = self else { return }
            if self.manager.roomState.roomVideoStreamIsLandscape { return }
            bottomMenu.disableFooterCoGuest(disableFooterCoGuest)
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
        rotateScreenDelegate?.rotateScreen(isPortrait: true)
        
        if !WindowUtils.isPortrait {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self] in
                guard let self = self else { return }
                self.manager.floatWindowSubject.send()
            }
            return
        }
        manager.floatWindowSubject.send()
    }
    
    @objc private func rotateScreenClick() {
        rotateScreenDelegate?.rotateScreen(isPortrait: !WindowUtils.isPortrait)
    }
    
    @objc func handleOrientationChange() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            activateConstraints()
            
            if WindowUtils.isPortrait {
                bottomMenu.isHidden = false
                barrageSendView.isHidden = false
            } else {
                bottomMenu.isHidden = true
                barrageSendView.isHidden = true
            }
        }
    }
        
    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        let view = super.hitTest(point, with: event)
        return view == self ? nil : view
    }
}

extension AudienceLivingView: BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: Barrage) -> UIView? {
        guard let type = barrage.extensionInfo?["TYPE"], type == "GIFTMESSAGE" else {
            return nil
        }
        return GiftBarrageCell(barrage: barrage)
    }
    
    func onBarrageClicked(user: LiveUserInfo) {
        if user.userID == manager.coreUserState.selfInfo.userId { return }
        routerManager.router(action: .present(.userManagement(TUIUserInfo(from: user), type: .userInfo)))
    }
}

extension AudienceLivingView: GiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: GiftPlayView, onReceiveGift gift: Gift, giftCount: Int, sender: LiveUserInfo) {
        let receiver = manager.coreRoomState.ownerInfo
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

extension AudienceLivingView {
    var barrageStore: BarrageStore {
        return BarrageStore.create(liveID: manager.roomState.roomId)
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
    static let meText = internalLocalized("Me")
    static let resolutionChangedText = internalLocalized("resolution changed to")
    
    static func videoQualityToString(quality: TUIVideoQuality) -> String {
        switch quality {
        case .quality1080P:
            return "1080P"
        case .quality720P:
            return "720P"
        case .quality540P:
            return "540P"
        case .quality360P:
            return "360P"
        default:
            return "Original"
        }
    }
}
