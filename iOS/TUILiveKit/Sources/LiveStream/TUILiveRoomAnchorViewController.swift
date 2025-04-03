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

@objcMembers
public class TUILiveRoomAnchorViewController: UIViewController {
    
    public var startLiveBlock:(()->Void)?
    
    // MARK: - private property.
    private lazy var manager: LiveStreamManager = LiveStreamManager(provider: self)
    private let routerManager: LSRouterManager = LSRouterManager()
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var likeManager = LikeManager(roomId: roomId)
    private lazy var coreView: LiveCoreView = {
        func setComponent() {
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
        }
        setComponent()
        return LiveCoreView()
    }()

    private let roomId: String
    private let needPrepare: Bool
    private lazy var routerCenter: LSRouterControlCenter = {
        let rootRoute: LSRoute = .anchor
        let routerCenter = LSRouterControlCenter(rootViewController: self, rootRoute: rootRoute, routerManager: routerManager, manager: manager, coreView: coreView)
        routerCenter.routerProvider = self
        return routerCenter
    }()
    
    private lazy var anchorView : AnchorView = {
        let view = AnchorView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: coreView)
        view.startLiveBlock = startLiveBlock
        return view
    }()
    
    public init(roomId: String, needPrepare: Bool = true, liveInfo: TUILiveInfo? = nil) {
        self.roomId = roomId
        self.needPrepare = needPrepare
        super.init(nibName: nil, bundle: nil)
        if FloatWindow.shared.isShowingFloatWindow() {
            FloatWindow.shared.releaseFloatWindow()
        }
        
        if let liveInfo = liveInfo {
            manager.prepareLiveInfoBeforeEnterRoom(liveInfo: liveInfo)
        } else {
            let liveInfo = TUILiveInfo()
            liveInfo.roomInfo.roomId = roomId
            liveInfo.coverUrl = manager.roomState.coverURL
            liveInfo.isPublicVisible = manager.roomState.liveExtraInfo.liveMode == .public
            liveInfo.activityStatus = manager.roomState.liveExtraInfo.activeStatus
            liveInfo.categoryList = [NSNumber(value: manager.roomState.liveExtraInfo.category.rawValue)]
            liveInfo.roomInfo.maxSeatCount = 9
            manager.prepareLiveInfoBeforeEnterRoom(liveInfo: liveInfo)
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        StateCache.shared.clear()
        print("deinit \(type(of: self))")
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
        view.backgroundColor = .black
        constructViewHierarchy()
        activateConstraints()
        subscribeSubjects()
        enableSubscribeRouter(enable: true)
        if !needPrepare {
            anchorView.joinSelfCreatedRoom()
        }
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

extension TUILiveRoomAnchorViewController {
    func constructViewHierarchy() {
        view.addSubview(anchorView)
    }
    
    func activateConstraints() {
        anchorView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
    }
    
    public func enableSubscribeRouter(enable: Bool) {
        enable ? routerCenter.subscribeRouter() : routerCenter.unSubscribeRouter()
    }
    
    private func subscribeSubjects() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                view.makeToast(message)
            }.store(in: &cancellableSet)
        
        manager.floatWindowSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                FloatWindow.shared.showFloatWindow(controller: self)
            }
            .store(in: &cancellableSet)
        
        manager.likeSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                likeManager.sendLike()
            }
            .store(in: &cancellableSet)
    }
}

extension TUILiveRoomAnchorViewController: LSRouterViewProvider {
    func getRouteView(route: LSRoute) -> UIView? {
        if route == .videoSetting {
            return VideoSettingPanel(routerManager: routerManager, manager: manager, coreView: coreView)
        } else {
            return nil
        }
    }
}

extension TUILiveRoomAnchorViewController: FloatWindowDataSource {
    func getRoomId() -> String {
        roomId
    }
    
    func getOwnerId() -> String {
        manager.coreRoomState.ownerInfo.userId
    }
    
    func getCoreView() -> LiveStreamCore.LiveCoreView {
        coreView
    }
    
    func relayoutCoreView() {
        anchorView.relayoutCoreView()
    }
    
    func getIsLinking() -> Bool {
        manager.coGuestState.coGuestStatus == .linking
    }
}

extension TUILiveRoomAnchorViewController: LiveStreamManagerProvider {
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        coreView.subscribeState(selector)
    }
    
    func getCoreViewState<T>() -> T where T : State {
        coreView.getState()
    }
}
