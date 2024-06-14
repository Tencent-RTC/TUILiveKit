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
    @Injected private var store: LiveStore
    @Injected private var viewStore: LiveRoomViewStore
    private var cancellableSet = Set<AnyCancellable>()
    private var popupViewController: UIViewController?
    private lazy var liveRouter: LiveRouter = {
         var liveRouter = LiveRouter(rootViewController: self, rootRoute: .anchor)
        liveRouter.viewProvider = self
        return liveRouter
    }()
    
    public var startLiveBlock:(()->Void)?
    
    private lazy var anchorView : AnchorView = {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let view = AnchorView(roomId: roomId)
        view.startLiveBlock = startLiveBlock
        return view
    }()
    
    public init(roomId:String) {
        super.init(nibName: nil, bundle: nil)
        self.store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        view.backgroundColor = .black
        initializeRoomState()
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
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        print("deinit \(type(of: self))")
    }
}

extension TUILiveRoomAnchorViewController {
    private func initializeRoomState() {
        liveRouter.subscribeRouter()
    }
    
    func constructViewHierarchy() {
        view.addSubview(anchorView)
    }
    func activateConstraints() {
        anchorView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
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

extension TUILiveRoomAnchorViewController: RouteViewProvider {
    func getRouteView(route: LiveRouter.Route) -> UIView? {
        if route == .musicList {
            return anchorView.musicPanelView
        } else {
            return nil
        }
    }
}
