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

@objcMembers
public class TUILiveRoomAnchorViewController: UIViewController {
    // MARK: - private property.
    private let manager = LiveStreamManager()
    private let routerManager: LSRouterManager = LSRouterManager()
    private var cancellableSet = Set<AnyCancellable>()
    
    private lazy var coreView: LiveCoreView = {
        let view = LiveCoreView()
        return view
    }()

    private let roomId: String
    private let needPrepare: Bool
    public var startLiveBlock:(()->Void)?
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
        manager.update { roomState in
            roomState.roomId = roomId
        }
        if needPrepare, let liveInfo = liveInfo {
            manager.updateRoomState(roomInfo: liveInfo.roomInfo)
            manager.update { roomState in
                roomState.coverURL = liveInfo.coverUrl
                roomState.liveExtraInfo.liveMode = liveInfo.isPublicVisible ? .public : .privacy
                if let category = liveInfo.categoryList.first {
                    roomState.liveExtraInfo.category = LiveStreamCategory(rawValue: category.intValue) ?? .chat
                }
            }
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        BeautyStoreFactory.removeStore(roomId: roomId)
        AudioEffectStoreFactory.removeStore(roomId: roomId)
        MusicPanelStoreFactory.removeStore(roomId: roomId)
        print("deinit \(type(of: self))")
    }

    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        view.backgroundColor = .black
        constructViewHierarchy()
        activateConstraints()
        subscribeToast()
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
    }

    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
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
    
    private func subscribeToast() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                view.makeToast(message)
            }.store(in: &cancellableSet)
    }
    
}

extension TUILiveRoomAnchorViewController: LSRouterViewProvider {
    func getRouteView(route: LSRoute) -> UIView? {
        if route == .beauty {
            return anchorView.beautyPanelView
        } else {
            return nil
        }
    }
}
