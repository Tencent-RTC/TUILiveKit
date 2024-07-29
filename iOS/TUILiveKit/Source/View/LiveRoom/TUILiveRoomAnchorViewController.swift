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

public class TUILiveRoomAnchorViewController: UIViewController {
    // MARK: - private property.
    private lazy var store: LiveStoreProvider = LiveStoreFactory.getStore(roomId: roomId)
    private let routerStore: RouterStoreProvider = RouterStoreProvider()
    private var cancellableSet = Set<AnyCancellable>()

    private let roomId: String
    public var startLiveBlock:(()->Void)?
    private lazy var routerCenter: RouterControlCenter = {
        let routerCenter = RouterControlCenter(rootViewController: self, store: store, rootRoute: .anchor, routerStore: routerStore)
        routerCenter.routerProvider = self
        return routerCenter
    }()
    private lazy var anchorView : AnchorView = {
        let view = AnchorView(roomId: roomId, routerStore: routerStore)
        view.startLiveBlock = startLiveBlock
        return view
    }()
    
    public init(roomId:String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
        store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        LiveRoomViewStoreFactory.removeStore(roomId: roomId)
        LiveStoreFactory.removeStore(roomId: roomId)
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
    
    func enableSubscribeRouter(enable: Bool) {
        enable ? routerCenter.subscribeRouter() : routerCenter.unSubscribeRouter()
    }
    
    private func subscribeToast() {
        store.toastSubject
            .removeDuplicates()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] toast in
                guard let self = self else { return }
                var position = TUICSToastPositionBottom
                switch toast.position {
                    case .center:
                        position = TUICSToastPositionCenter
                    default:
                        break
                }
                self.view.makeToast(toast.message, duration: toast.duration, position: position)
            }
            .store(in: &cancellableSet)
    }
    
}

extension TUILiveRoomAnchorViewController: RouterViewProvider {
    func getRouteView(route: Route) -> UIView? {
        if route == .beauty {
            return anchorView.beautyPanelView
        } else {
            return nil
        }
    }
}
