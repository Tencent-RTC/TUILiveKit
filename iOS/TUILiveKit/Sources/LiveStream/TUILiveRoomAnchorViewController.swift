//
//  TUILiveRoomAnchorViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import TUICore
import RTCRoomEngine
import Combine
import AtomicXCore
import RTCCommon

@objcMembers
public class TUILiveRoomAnchorViewController: UIViewController {
    
    public var startLiveBlock:(()->Void)?

    // MARK: - private property.
    private var cancellableSet = Set<AnyCancellable>()
    private let coreView: LiveCoreView

    private let liveInfo: LiveInfo
    private let behavior: RoomBehavior
    
    private let anchorView: AnchorView
    
    public init(liveInfo: LiveInfo, coreView: LiveCoreView? = nil, behavior: RoomBehavior = .createRoom) {
        self.liveInfo = liveInfo
        self.behavior = behavior
        if let coreView = coreView {
            self.coreView = coreView
        } else {
            do {
                let jsonObject: [String: Any] = [
                    "api": "component",
                    "component": 21
                ]
                let jsonData = try JSONSerialization.data(withJSONObject: jsonObject, options: [])
                if let jsonString = String(data: jsonData, encoding: .utf8) {
                    LiveCoreView.callExperimentalAPI(jsonString)
                }
            } catch {
                LiveKitLog.error("\(#file)","\(#line)", "dataReport: \(error.localizedDescription)")
            }
            self.coreView = LiveCoreView(viewType: .pushView)
        }
        self.anchorView = AnchorView(liveInfo: liveInfo, coreView: self.coreView, behavior: behavior)
        super.init(nibName: nil, bundle: nil)
        if FloatWindow.shared.isShowingFloatWindow() {
            FloatWindow.shared.releaseFloatWindow()
        }
        
        anchorView.delegate = self
        anchorView.startLiveBlock = startLiveBlock
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        StateCache.shared.clear()
        AudioEffectStore.shared.reset()
        DeviceStore.shared.reset()
        BaseBeautyStore.shared.reset()
        LiveKitLog.info("\(#file)", "\(#line)", "deinit TUILiveRoomAnchorViewController \(self)")
#if DEV_MODE
        TestTool.shared.unregisterCaseFrom(self)
#endif
    }
    
    public func stopLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        coreView.stopLiveStream(onSuccess: {
            onSuccess?()
        }, onError: { code, message in
            onError?(code, message)
        })
    }

    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        startLiveBlock?()
        
#if DEV_MODE
        let liveData = TestCaseItemModel(title: "禁用房间信息", view: anchorView, sel: #selector(AnchorView.disableHeaderLiveDataForTest(_:)))
        let visitor = TestCaseItemModel(title: "禁用观众列表", view: anchorView, sel: #selector(AnchorView.disableHeaderVisitorCntForTest(_:)))
        let coGuest = TestCaseItemModel(title: "禁用连麦按钮", view: anchorView, sel: #selector(AnchorView.disableFooterCoGuestForTest(_:)))
        let coHost = TestCaseItemModel(title: "禁用连线按钮", view: anchorView, sel: #selector(AnchorView.disableFooterCoHostForTest(_:)))
        let battle = TestCaseItemModel(title: "禁用PK按钮", view: anchorView, sel: #selector(AnchorView.disableFooterBattleForTest(_:)))
        let soundEffect = TestCaseItemModel(title: "禁用音效按钮", view: anchorView, sel: #selector(AnchorView.disableFooterSoundEffectForTest(_:)))
        let model = TestCaseModel(list: [liveData, visitor, coGuest, coHost, battle, soundEffect], obj: self)
        TestTool.shared.registerCase(model)
#endif
    }
    
    public override func loadView() {
        view = anchorView
    }

    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        view.endEditing(true)
    }

    public override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
        super.viewWillTransition(to: size, with: coordinator)
        let isPortrait = size.width < size.height
        anchorView.updateRootViewOrientation(isPortrait: isPortrait)
    }

    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        UIApplication.shared.isIdleTimerDisabled = true
        navigationController?.setNavigationBarHidden(true, animated: true)
    }

    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
        navigationController?.setNavigationBarHidden(false, animated: true)
    }
    
    public override var shouldAutorotate: Bool {
        return false
    }
    
    public override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .portrait
    }
}

extension TUILiveRoomAnchorViewController: AnchorViewDelegate {
    public func onClickFloatWindow() {
        FloatWindow.shared.showFloatWindow(controller: self, provider: self)
    }
    
    public func onEndLiving(state: AnchorState) {
        let liveDataModel = AnchorEndStatisticsViewInfo(roomId: liveInfo.roomId,
                                                        liveDuration: state.duration,
                                                        viewCount: state.viewCount,
                                                        messageCount: state.messageCount,
                                                        giftTotalCoins: state.giftTotalCoins,
                                                        giftTotalUniqueSender: state.giftTotalUniqueSender,
                                                        likeTotalUniqueSender: state.likeTotalUniqueSender)
        let anchorEndView = AnchorEndStatisticsView(endViewInfo: liveDataModel)
        anchorEndView.delegate = self
        view.addSubview(anchorEndView)
        anchorEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

extension TUILiveRoomAnchorViewController: FloatWindowProvider {
    public func getRoomId() -> String {
        liveInfo.roomId
    }
    
    public func getOwnerId() -> String {
        let roomState: RoomState = coreView.getState()
        return roomState.ownerInfo.userId
    }
    
    public func getCoreView() -> AtomicXCore.LiveCoreView {
        coreView
    }
    
    public func relayoutCoreView() {
        anchorView.relayoutCoreView()
    }
    
    public func getIsLinking() -> Bool {
        let coGuestState: CoGuestState = coreView.getState()
        let userState: UserState = coreView.getState()
        return coGuestState.connectedUserList.contains(where: { $0.userId == userState.selfInfo.userId })
    }
}

extension TUILiveRoomAnchorViewController: AnchorEndStatisticsViewDelegate {
    func onCloseButtonClick() {
        if let nav = navigationController {
            nav.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
    }
}
