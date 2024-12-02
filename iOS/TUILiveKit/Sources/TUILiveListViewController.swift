//
//  TUILiveListViewController.swift
//  Alamofire
//
//  Created by adamsfliu on 2024/5/30.
//

import UIKit
import RTCCommon
import TUICore
import Combine

public class TUILiveListViewController: UIViewController {
    private let liveListStore: LiveListStoreProvider = LiveListStoreProvider()
    private lazy var currentRouterPublisher = self.liveListStore.select(LiveListSelectors.getCurrentRouter)
    
    // MARK: - Internal property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    
    private lazy var rootView: LiveListRootView = {
        let view = LiveListRootView(store: liveListStore)
        return view
    }()
    
    private var cancellableSet = Set<AnyCancellable>()
    private var popupViewController: UIViewController?
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        initNavigationItemTitleView()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        view.backgroundColor = .g8
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
    
    override public func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        rootView.refreshRoomListData()
    }
    
    func constructViewHierarchy() {
        view.addSubview(rootView)
    }
    
    func activateConstraints() {
        rootView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func bindInteraction() {
        subscribeNavigationState()
    }
    
    private func initNavigationItemTitleView() {
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(UIImage(named: "live_back"), for: .normal)
        backBtn.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        backBtn.sizeToFit()
        let backItem = UIBarButtonItem(customView: backBtn)
        backItem.tintColor = .black
        navigationItem.leftBarButtonItem = backItem
        
        let titleView = UILabel()
        titleView.text = .liveTitleText
        titleView.textColor = .black
        titleView.textAlignment = .center
        titleView.font = UIFont.boldSystemFont(ofSize: 17)
        titleView.adjustsFontSizeToFitWidth = true
        let width = titleView.sizeThatFits(CGSize(width: CGFloat.greatestFiniteMagnitude,
                                                  height: CGFloat.greatestFiniteMagnitude)).width
        titleView.frame = CGRect(origin:CGPoint.zero, size:CGSize(width: width, height: 500))
        self.navigationItem.titleView = titleView
        
        let helpBtn = UIButton(type: .custom)
        helpBtn.setImage(UIImage(named: "help_small"), for: .normal)
        helpBtn.addTarget(self, action: #selector(helpBtnClick), for: .touchUpInside)
        helpBtn.sizeToFit()
        let rightItem = UIBarButtonItem(customView: helpBtn)
        rightItem.tintColor = .black
        navigationItem.rightBarButtonItem = rightItem
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension TUILiveListViewController {
    @objc
    private func backBtnClick(sender: UIButton) {
        liveListStore.dispatch(action: LiveListNavigatorActions.navigatorTo(payload: .exit))
    }
    
    @objc
    private func helpBtnClick(sender: UIButton) {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }

}

extension TUILiveListViewController {
    
    private func subscribeNavigationState() {
        currentRouterPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] router in
                guard let self = self else { return }
                switch router {
                case .exit:
                    self.navigationController?.popViewController(animated: true)
                case .main:
                    break
                case let .toLive(liveInfo):
                    if FloatWindow.shared.isShowingFloatWindow() {
                        if FloatWindow.shared.getCurrentRoomId() == liveInfo.roomInfo.roomId {
                            FloatWindow.shared.resumeLive(atViewController: self.navigationController ?? self)
                            return
                        } else {
                            FloatWindow.shared.releaseFloatWindow()
                        }
                    }
                    let roomType = LiveIdentityGenerator.shared.getIDType(liveInfo.roomInfo.roomId)
                    let isOwner = liveInfo.roomInfo.ownerId == TUILogin.getUserID()
                    switch roomType {
                    case .voice:
                        let vc = TUIVoiceRoomViewController(roomId: liveInfo.roomInfo.roomId, behavior: isOwner ? .autoCreate : .join)
                        self.navigationController?.pushViewController(vc, animated: true)
                    case .live:
                        if isOwner {
                            let vc = TUILiveRoomAnchorViewController(roomId: liveInfo.roomInfo.roomId, needPrepare: false)
                            self.navigationController?.pushViewController(vc, animated: true)
                        } else {
                            let vc = TUILiveRoomAudienceViewController(roomId: liveInfo.roomInfo.roomId)
                            self.navigationController?.pushViewController(vc, animated: true)
                        }
                        break
                    default: break
                    }
                }
            }
            .store(in: &cancellableSet)
    }
}

extension String {
    fileprivate static let liveTitleText = localized("live.room.list.live")
}
