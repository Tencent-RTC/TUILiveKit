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
    
    // MARK: - Private property.
    private var needRestoreNavigationBarHiddenState: Bool = false
    
    @Injected private var store: LiveStore
    @Injected private var viewStore: VoiceRoomViewStore
    @Injected private var routerStore: RouterStore
    
    private lazy var routerCenter: RouterControlCenter = {
        let rootRoute: Route = behavior == .join ? .audience : .anchor
        let routerCenter = RouterControlCenter(rootViewController: self, rootRoute: rootRoute)
        return routerCenter
    }()
    
    private lazy var voiceRootView: VoiceRoomRootView = {
        let view = VoiceRoomRootView(frame: UIScreen.main.bounds,
                                     roomId: roomId,
                                     roomParams: roomParams)
        return view
    }()
    
    private lazy var voicePrepareView: VoiceRoomPrepareView = {
        let view = VoiceRoomPrepareView(frame: UIScreen.main.bounds)
        view.delegate = self
        return view
    }()
    
    private lazy var liveStatusPublisher = self.store.select(ViewSelectors.getLiveStatus)
    
    private let roomId: String
    private var cancellableSet = Set<AnyCancellable>()
    private var enterRoomCancellable: AnyCancellable?
    
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
        handle(behavior: behavior)
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
    
    private func handle(behavior: RoomBehavior) {
        switch behavior {
            case .join, .autoCreate:
                showVoiceRoot()
                subscribeViewState()
                enableSubscribeRouter(enable: true)
            case .prepareCreate:
                showVoicePrepare()
        }
    }
    
    private func showVoicePrepare() {
        view.addSubview(voicePrepareView)
        voicePrepareView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
    }
    
    private func showVoiceRoot() {
        view.addSubview(voiceRootView)
        voiceRootView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
        routerCenter.routerProvider = voiceRootView
    }
}

// MARK: - Store
extension TUIVoiceRoomViewController {
    func enableSubscribeRouter(enable: Bool) {
        enable ? routerCenter.subscribeRouter() : routerCenter.unSubscribeRouter()
    }
}

// MARK: - Subscribe state
extension TUIVoiceRoomViewController {
    
    func subscribeViewState() {
        subscribeEnterRoomState()
        subscribeToast()
    }
    
    func unSubscribeViewState() {
        enterRoomCancellable?.cancel()
        enterRoomCancellable = nil
    }
    
    private func subscribeRouter() {
        routerCenter.subscribeRouter()
    }
    
    private func subscribeEnterRoomState() {
        guard enterRoomCancellable == nil else { return }
        enterRoomCancellable = liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                case .previewing:
                    voicePrepareView.isHidden = false
                case .playing, .pushing:
                    voicePrepareView.isHidden = true
                    voiceRootView.didEnterRoom()
                case .finished:
                    voicePrepareView.isHidden = true
                case .none:
                    routerStore.router(action: .exit)
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
}

extension TUIVoiceRoomViewController: VoiceRoomPrepareViewDelegate {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton) {
        startLiveClosure?()
        showVoiceRoot()
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
