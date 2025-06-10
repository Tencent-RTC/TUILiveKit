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
import LiveStreamCore
import RTCCommon
import TUILiveResources

@objcMembers
public class TUILiveRoomAnchorViewController: UIViewController {
    
    public var startLiveBlock:(()->Void)?

    // MARK: - private property.
    private var cancellableSet = Set<AnyCancellable>()
    private let coreView: LiveCoreView

    private let roomId: String
    private let needPrepare: Bool
    
    private let anchorView: AnchorView
    
    public init(roomId: String, needPrepare: Bool = true, liveInfo: TUILiveInfo? = nil, coreView: LiveCoreView? = nil) {
        self.roomId = roomId
        self.needPrepare = needPrepare
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
            self.coreView = LiveCoreView()
        }
        self.anchorView = AnchorView(roomId: roomId, coreView: self.coreView, liveInfo: liveInfo)
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
        LiveKitLog.info("\(#file)", "\(#line)", "deinit TUILiveRoomAnchorViewController \(self)")
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
        if !needPrepare {
            startLiveBlock?()
            anchorView.joinSelfCreatedRoom()
        }
        GiftCloudServer.shared.initialize()
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
    
    func startLiveStream(roomName: String? = nil,
                         privacyMode: LiveStreamPrivacyStatus? = nil,
                         coverUrl: String? = nil) {
        startLiveBlock?()
        anchorView.startLiveStream(roomName: roomName, privacyMode: privacyMode, coverUrl: coverUrl)
    }
}

extension TUILiveRoomAnchorViewController: AnchorViewDelegate {
    public func showFloatWindow() {
        FloatWindow.shared.showFloatWindow(controller: self)
    }
}

extension TUILiveRoomAnchorViewController: FloatWindowDataSource {
    public func getRoomId() -> String {
        roomId
    }
    
    public func getOwnerId() -> String {
        let roomState: RoomState = coreView.getState()
        return roomState.ownerInfo.userId
    }
    
    public func getCoreView() -> LiveStreamCore.LiveCoreView {
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
