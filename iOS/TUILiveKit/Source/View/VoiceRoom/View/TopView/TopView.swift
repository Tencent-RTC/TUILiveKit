//
//  TopView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import UIKit
import SnapKit
import Kingfisher
import Combine

private let containerHeight = 36.0
private let componentHeight = 32.0

protocol TopViewDelegate: AnyObject {
    func topView(_ topView: TopView, tap event:TopView.TapEvent, sender: Any?) -> Void
}

class TopView: UIView {
    
    enum TapEvent {
        case stop
        case audienceList
        case roomInfo
    }
    
    private var store: LiveStore
    private var routerStore: RouterStore
    
    private lazy var isOwnerPublisher = self.store.select(UserSelectors.isOwner)
    var cancellableSet: Set<AnyCancellable> = []
    
    weak var delegate: TopViewDelegate?
   
    private lazy var roomInfoView: RoomInfoView = {
        let view = RoomInfoView(store: store)
        view.mm_h = Int(componentHeight).scale375()
        view.backgroundColor = UIColor.g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()
    
    private lazy var audienceListView: AudienceListView = {
        let view = AudienceListView(store: store, routerStore: routerStore)
        return view
    }()
    
    let stopButton: UIButton = {
        let button = UIButton(type: .system)
        button.setBackgroundImage(.liveBundleImage( "live_leave_icon"), for: .normal)
        return button
    }()
    
    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(roomInfoView)
        addSubview(audienceListView)
        addSubview(stopButton)
    }
    
    private func activeViewConstraint() {
        roomInfoView.snp.remakeConstraints { make in
            make.centerY.equalToSuperview()
            make.height.equalTo(roomInfoView.mm_h)
            make.width.greaterThanOrEqualTo(80.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset(16.scale375())
            make.top.bottom.equalToSuperview()
        }
        
        audienceListView.snp.remakeConstraints { make in
            make.centerY.equalTo(stopButton)
            make.height.equalTo(24.scale375())
            make.width.equalTo(116.scale375())
            make.trailing.equalTo(stopButton.snp.leading).offset(-4.scale375())
        }
      
        stopButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.right.equalToSuperview().offset(-16)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }
    }
    
    private func bindInteraction() {
        stopButton.addTarget(self, action: #selector(buttonTouchUpInSide(sender:)), for: .touchUpInside)
    
        let roomInfoViewTap = UITapGestureRecognizer(target: self, action: #selector(roomInfoViewTap(sender:)))
        roomInfoView.addGestureRecognizer(roomInfoViewTap)
    }
}

extension TopView {
    @objc
    private func buttonTouchUpInSide(sender: UIButton) {
        self.delegate?.topView(self, tap: .stop, sender: sender)
    }
    
    @objc func roomInfoViewTap(sender: UITapGestureRecognizer) {
        self.delegate?.topView(self, tap: .roomInfo, sender: sender)
    }
}
