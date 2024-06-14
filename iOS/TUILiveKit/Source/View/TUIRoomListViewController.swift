//
//  TUIRoomListViewController.swift
//  Alamofire
//
//  Created by adamsfliu on 2024/5/30.
//

import UIKit
import RTCCommon
import TUICore
import Combine

public class TUIRoomListViewController: UIViewController {
    @Injected private var store: LiveStore
    @Injected private var roomListStore: RoomListStoreProvider
    private lazy var currentRouterPublisher = self.roomListStore.select( RoomListSelectors.getCurrentRouter)
    
    // MARK: - Internal property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    
    private lazy var rootView: RoomListRootView = {
        let view = RoomListRootView(frame: .zero)
        return view
    }()
    
    private var cancellableSet = Set<AnyCancellable>()
    private var popupViewController: UIViewController?
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: false)
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
        subscribeToast()
        subscribeNavigationState()
    }
    
    deinit {
        RoomListRootView.session.reset()
        print("deinit \(type(of: self))")
    }
}

extension TUIRoomListViewController {
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
    
    private func subscribeNavigationState() {
        currentRouterPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] router in
                guard let self = self else { return }
                switch router {
                case .exit:
                    self.navigationController?.popViewController(animated: true)
                case .main:
                    popupViewController?.dismiss(animated: true)
                    popupViewController = nil
                case let .toLive(liveInfo):
                    let roomId = liveInfo.roomInfo.roomId
                    guard let roomType = LiveIdentityGenerator.shared.getIDType(roomId)  else {
                        let viewController = TUILiveRoomAudienceViewController(roomId: roomId)
                        self.navigationController?.pushViewController(viewController, animated: true)
                        return
                    }
                    
                    var viewController: UIViewController
                    switch roomType {
                    case .live:
                        viewController = TUILiveRoomAudienceViewController(roomId: roomId)
                    case .voice:
                        viewController = TUIVoiceRoomViewController(roomId: roomId, behavior: .join)
                    }
                    self.navigationController?.pushViewController(viewController, animated: true)
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func presentPopup(view: UIView) {
        if let vc = popupViewController {
            vc.dismiss(animated: false)
            popupViewController = nil
        }
        let menuContainerView = MenuContainerView(contentView: view)
        menuContainerView.blackAreaClickClosure = { [weak self] in
            guard let self = self else { return }
            self.popMenu()
        }
        let viewController = PopupViewController(contentView: menuContainerView)
        present(viewController, animated: true)
        popupViewController = viewController
    }
    
    private func popMenu() {
        roomListStore.dispatch(action: RoomListNavigatorActions.navigatorTo(payload: .main))
    }
}
