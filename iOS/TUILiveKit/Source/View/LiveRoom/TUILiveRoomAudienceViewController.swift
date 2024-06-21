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
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let view = AudienceView(roomId: roomId)
        return view
    }()
    
    // MARK: - private property.
    @Injected private var store: LiveStore
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var routerCenter: RouterControlCenter = {
        let routerCenter = RouterControlCenter(rootViewController: self, rootRoute: .audience)
        routerCenter.routerProvider = audienceView
        return routerCenter
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
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        print("deinit \(type(of: self))")
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
