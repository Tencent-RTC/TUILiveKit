//
//  TUIVoiceRoomViewController.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import UIKit
import RTCCommon
import Combine
import RTCRoomEngine
import TUICore

public struct RoomParams {
    public var maxSeatCount: Int = 0 //The default value is the maximum number of seat supported by the package
    public var seatMode: TUISeatMode = .applyToTake
    public init(maxSeatCount: Int, seatMode: TUISeatMode) {
        self.maxSeatCount = maxSeatCount
        self.seatMode = seatMode
    }
    public init() {}
}

public class TUIVoiceRoomViewController: UIViewController {
    
    public enum RoomBehavior {
        case autoCreate
        case prepareCreate
        case join
    }
    public typealias OnStartClosure = () -> Void
    
    // MARK: - Public property.
    public var behavior: RoomBehavior = .prepareCreate
    public var roomParams: RoomParams?
    public var startLiveClosure: OnStartClosure?
    
    // MARK: - Internal property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    
    @Injected private var store: LiveStore
    @Injected private var viewStore: VoiceRoomViewStore
    // MARK: - Private property.
    private lazy var currentRouterPublisher = self.viewStore.select( VoiceRoomViewSelectors.getCurrentRouter)
    private lazy var liveStatusPublisher = self.store.select(ViewSelectors.getLiveStatus)
    
    private lazy var prepareView: VoiceRoomPrepareView = {
        let view = VoiceRoomPrepareView(frame: UIScreen.main.bounds)
        view.delegate = self
        return view
    }()
    private lazy var rootView: VoiceRoomRootView = {
        return VoiceRoomRootView(frame: UIScreen.main.bounds)
    }()
    private var cancellableSet = Set<AnyCancellable>()
    private var enterRoomCancellable: AnyCancellable?
    private var popupViewController: UIViewController?
    
    // MARK: - Public function
    public init(roomId: String, behavior: RoomBehavior, roomParams: RoomParams? = nil) {
        self.behavior = behavior
        self.roomParams = roomParams
        super.init(nibName: nil, bundle: nil)
        self.store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        initializeOnViewDidiLoad()
        subscribeVoiceRoomNavigationState()
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
        unSubscribeViewState()
        print("deinit \(type(of: self))")
    }
    
    // MARK: - Private Function
    private func initializeOnViewDidiLoad() {
        initializeRoomState()
        handle(behavior: behavior)
    }
    
    func subscribeViewState() {
        subscribeEnterRoomState()
        subscribeToast()
    }
    
    func unSubscribeViewState() {
        enterRoomCancellable?.cancel()
        enterRoomCancellable = nil
    }
    
    private func handle(behavior: RoomBehavior) {
        switch behavior {
            case .join:
                subscribeViewState()
                join()
            case .autoCreate:
                subscribeViewState()
                start()
            case .prepareCreate:
                showPrepare()
        }
    }
    
    private func join() {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let param = generateActionParamTuple(param: roomId, actions: [])
        DataReporter.componentType = .voiceRoom
        store.dispatch(action: RoomActions.join(payload: param))
    }
    
    private func start() {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = roomId
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        guard let roomParams = roomParams else { return }
        roomInfo.name = store.selectCurrent(UserSelectors.getSelfInfo).name
        roomInfo.seatMode = roomParams.seatMode
        roomInfo.maxSeatCount = roomParams.maxSeatCount
        let config = generateActionParamTuple(param: roomInfo, actions: [])
        DataReporter.componentType = .voiceRoom
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
        store.dispatch(action: ViewActions.updateLiveStatus(payload: .previewing))
        store.dispatch(action: UserActions.getSelfInfo())
    }
}

// MARK: - Subscribe state
extension TUIVoiceRoomViewController {
    private func subscribeVoiceRoomNavigationState() {
        viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .main))
        currentRouterPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] menu in
                guard let self = self else { return }
                self.handleMenuPresentEvent(menu: menu)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeEnterRoomState() {
        guard enterRoomCancellable == nil else { return }
        enterRoomCancellable = liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .playing,.pushing:
                        self.didEnterRoom()
                        self.startLiveClosure?()
                    case .none:
                        viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .exit))
                    default:
                        break
                }
            }
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
            RoomActions.fetchRoomOwnerInfo(),
        ]
        if store.selectCurrent(UserSelectors.isOwner) {
            actions.append(SeatActions.takeSeat(payload: nil))
            actions.append(MediaActions.operateMicrophone(payload: true))
        } else {
            actions.append(UserActions.checkFollowType(payload: store.selectCurrent(RoomSelectors.roomOwnerId)))
        }
        actions.forEach { action in
            store.dispatch(action: action)
        }
        viewStore.dispatch(action: VoiceRoomViewActions.updateBottomMenus())
    }
    
    private func handleMenuPresentEvent(menu: VoiceRoomNavigationState.Router) {
        switch menu {
            case .exit:
                self.navigationController?.popViewController(animated: true)
            case .main:
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            case let .listMenu(menus):
                let view = ActionPanel(items: menus)
                view.cancelActionClosure = { [weak self] in
                    guard let self = self else { return }
                    self.popMenu()
                }
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
            case .roomInfoPanel:
                let view = RoomInfoPanelView()
                presentPopup(view: view)
            case .systemImageSelection:
                let view = SystemImageSelectionPanel(configs: SystemImageModel.configs())
                view.backButtonClickClosure = { [weak self] in
                    guard let self = self else { return }
                    self.popMenu()
                }
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
        viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .main))
    }
}

extension TUIVoiceRoomViewController: VoiceRoomPrepareViewDelegate {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton) {
        start()
    }
}
