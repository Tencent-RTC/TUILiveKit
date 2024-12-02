//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//
import UIKit
import Combine
import TUICore
import LiveStreamCore

public class TUILiveRoomAudienceViewController: UIViewController {
    
    private lazy var audienceView: AudienceView = {
        let view = AudienceView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: coreView)
        view.livingView.onButtonTap = { [weak self] in
            guard let self = self else { return }
            FloatWindow.shared.showFloatWindow(controller: self)
        }
        return view
    }()
    
    private lazy var coreView: LiveCoreView = {
        let view = LiveCoreView()
//        view.delegate = self
        return view
    }()
    
    // MARK: - private property.
    let roomId: String
    private let manager = LiveStreamManager()
    private let routerManager: LSRouterManager = LSRouterManager()
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var routerCenter: LSRouterControlCenter = {
        let rootRoute: LSRoute = .audience
        let routerCenter = LSRouterControlCenter(rootViewController: self, rootRoute: rootRoute, routerManager: routerManager, manager: manager, coreView: coreView)
        routerCenter.routerProvider = audienceView
        return routerCenter
    }()
    
    public init(roomId:String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
//        LiveStoreFactory.removeStore(roomId: roomId)
//        LiveRoomViewStoreFactory.removeStore(roomId: roomId)
        print("deinit \(type(of: self))")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        subscribeRouter()
        constructViewHierarchy()
        activateConstraints()
        subscribeToast()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: true)
        UIApplication.shared.isIdleTimerDisabled = true
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
        navigationController?.setNavigationBarHidden(false, animated: true)
    }
}

extension TUILiveRoomAudienceViewController {
    private func subscribeRouter() {
        routerCenter.subscribeRouter()
    }
    
    private func subscribeToast() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                view.makeToast(message)
            }.store(in: &cancellableSet)
    }
    
    private func constructViewHierarchy() {
        view.backgroundColor = .g1
        view.addSubview(audienceView)
    }
    
    private func activateConstraints() {
        audienceView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

// MARK: - FloatWindowDataSource
extension TUILiveRoomAudienceViewController: FloatWindowDataSource {
    func getRoomId() -> String {
        roomId
    }

    func getCoreView() -> LiveCoreView {
        coreView
    }
    
    func relayoutCoreView() {
        audienceView.relayoutCoreView()
    }
}
