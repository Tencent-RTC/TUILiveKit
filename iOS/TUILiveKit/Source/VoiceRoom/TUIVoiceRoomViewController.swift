//
//  TUIVoiceRoomViewController.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import UIKit

import Combine
import RTCRoomEngine
import TUICore

public struct RoomParams {
    public var maxSeatCount: Int = 0 //The default value is the maximum number of seat supported by the package
    public var seatMode: TUISeatMode = .applyToTake
}

public class TUIVoiceRoomViewController: UIViewController {
    
    public enum RoomBehavior {
        case autoCreate
        case prepareCreate
        case join
    }
    public typealias OnStartClosure = () -> Void
    
    // MARK: - Public property.
    public var roomId: String = ""
    public var behavior: RoomBehavior = .prepareCreate
    public var roomParams: RoomParams?
    public var startLiveClosure: OnStartClosure?
    
    // MARK: - Internal property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    @Injected private var store: VoiceRoomStoreProvider
    
    // MARK: - Private property.
    private lazy var currentRouter = self.store.select( ViewSelectors.currentRouterSelector)
    private lazy var enterRoomState = self.store.select(RoomSelectors.getEnterRoomState)
    
    private lazy var prepareView: VoiceRoomPrepareView = {
        let view = VoiceRoomPrepareView(frame: UIScreen.main.bounds)
        view.delegate = self
        return view
    }()
    private lazy var rootView: VoiceRoomRootView = {
        return VoiceRoomRootView(frame: UIScreen.main.bounds)
    }()
    private var cancellableSet = Set<AnyCancellable>()
    private var popupViewController: UIViewController?
    
    // MARK: - Public function
    public init(roomId: String, behavior: RoomBehavior, roomParams: RoomParams? = nil) {
        self.roomId = roomId
        self.behavior = behavior
        self.roomParams = roomParams
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        initializeOnViewDidiLoad()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        hideNavigationBar()
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        restoreNavigationBar()
    }
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        store.dispatch(action: RoomActions.leaveSuccess())
        print("deinit \(type(of: self))")
    }
    
    // MARK: - Private Function
    private func initializeOnViewDidiLoad() {
        initializeRoomState()
        subscribeNavigationState()
        subscribeToast()
        handle(behavior: behavior)
        subscribeEnterRoomState()
    }
    
    private func handle(behavior: RoomBehavior) {
        switch behavior {
            case .join:
                join()
            case .autoCreate:
                start()
            case .prepareCreate:
                showPrepare()
        }
    }
    
    private func join() {
        let param = generateActionParamTuple(param: roomId, actions: [])
        store.dispatch(action: RoomActions.join(payload: param))
    }
    
    private func start() {
        let roomState = store.selectCurrent(RoomSelectors.getRoomState)
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = roomState.roomId
        roomInfo.name = roomState.name
        roomInfo.isSeatEnabled = true
        roomInfo.seatMode = roomState.seatMode
        roomInfo.maxSeatCount = roomState.seatCount
        roomInfo.roomType = .live
        let config = generateActionParamTuple(param: roomInfo, actions: [])
        store.dispatch(action: RoomActions.start(payload: config))
    }
    
    private func showPrepare() {
        view.addSubview(prepareView)
        prepareView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
    }
    
    func constructViewHierarchy() {
        view.addSubview(rootView)
    }
    
    func activateConstraints() {
        rootView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
    }
}
// MARK: - ViewController navigation style record.
extension TUIVoiceRoomViewController {
    private func hideNavigationBar() {
        guard let navigationController = self.navigationController, !navigationController.isNavigationBarHidden else { return }
        needRestoreNavigationBarHiddenState = true
        navigationController.setNavigationBarHidden(true, animated: false)
    }
    
    private func restoreNavigationBar() {
        guard let navigationController = self.navigationController, needRestoreNavigationBarHiddenState else { return }
        navigationController.setNavigationBarHidden(false, animated: true)
        needRestoreNavigationBarHiddenState = false
    }
}

// MARK: - Store
extension TUIVoiceRoomViewController {
    private func initializeRoomState() {
        store.dispatch(action: UserActions.getSelfInfo())
        let currentUserId = store.selectCurrent(UserSelectors.currentUserId)
        var state = RoomState()
        state.roomId = roomId
        if behavior != .join {
            state.ownerId = currentUserId
        }
        
        guard let roomParams = roomParams else { return }
        state.name = store.selectCurrent(UserSelectors.getSelfInfo).name
        state.seatMode = roomParams.seatMode
        state.seatCount = roomParams.maxSeatCount
        store.dispatch(action: RoomActions.initializeRoomState(payload: state))
    }
}

// MARK: - Subscribe state
extension TUIVoiceRoomViewController {
    private func subscribeNavigationState() {
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
        currentRouter
            .receive(on: RunLoop.main)
            .sink { [weak self] menu in
                guard let self = self else { return }
                self.handleMenuPresentEvent(menu: menu)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeEnterRoomState() {
        enterRoomState
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                switch state {
                    case .inRoom:
                        self.didEnterRoom()
                        self.startLiveClosure?()
                    default:
                        break
                }
            }
            .store(in: &cancellableSet)
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
    
    private func didEnterRoom() {
        var actions:[Action] = [
            SeatActions.fetchSeatList(),
            UserActions.fetchUserList(),
            UserActions.fetchRoomOwnerInfo(),
        ]
        if store.selectCurrent(UserSelectors.isOwner) {
            actions.append(SeatActions.takeSeat(payload: nil))
            actions.append(MediaActions.operateMicrophone(payload: true))
        }
        actions.forEach { action in
            store.dispatch(action: action)
        }
        store.dispatch(action: ViewActions.updateBottomMenus())
    }
    
    private func handleMenuPresentEvent(menu: NavigationState.Router) {
        switch menu {
            case .exit:
                self.navigationController?.popViewController(animated: true)
            case .main:
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            case let .listMenu(menus):
                let view = ListMenuView(menus: menus)
                presentPopup(view: view)
            case .seatApplication:
                let view = SeatApplicationListView(frame: .zero)
                presentPopup(view: view)
            case .audioEffectPanel:
                let view = AudioEffectView(frame: .zero)
                view.backButtonClickClosure = { [weak self] _ in
                    guard let self = self else { return }
                    self.popMenu()
                }
                presentPopup(view: view)
            case .musicListPanel:
                presentPopup(view: rootView.musicPanelView)
            case .giftList:
                presentPopup(view: rootView.giftListView)
            case .audienceList:
                let view = VoiceAudienceListView()
                presentPopup(view: view)
        }
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
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
    }
}

extension TUIVoiceRoomViewController: VoiceRoomPrepareViewDelegate {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton) {
        start()
    }
}
