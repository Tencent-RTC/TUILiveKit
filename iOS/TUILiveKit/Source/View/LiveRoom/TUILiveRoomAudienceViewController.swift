//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//
import UIKit
import Combine
import TUICore

public class TUILiveRoomAudienceViewController: UIViewController {
    
    private lazy var audienceView: AudienceView = {
        let view = AudienceView(roomId: roomId, routerStore: routerStore)
        return view
    }()
    
    // MARK: - private property.
    let roomId: String
    private lazy var store: LiveStoreProvider = LiveStoreFactory.getLiveStore(roomId: roomId)
    private let routerStore: RouterStoreProvider = RouterStoreProvider()
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var routerCenter: RouterControlCenter = {
        let routerCenter = RouterControlCenter(rootViewController: self, store: store, rootRoute: .audience, routerStore: routerStore)
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
        LiveStoreFactory.removeLiveStore(roomId: roomId)
        LiveRoomViewStoreFactory.removeLiveRoomViewStore(roomId: roomId)
        print("deinit \(type(of: self))")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        subscribeRouter()
        constructViewHierarchy()
        activateConstraints()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        UIApplication.shared.isIdleTimerDisabled = true
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
    }
}

extension TUILiveRoomAudienceViewController {
    private func subscribeRouter() {
        routerCenter.subscribeRouter()
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
