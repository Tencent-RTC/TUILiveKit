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
import LiveStreamCore

let defaultMaxSeatCount = 10

@objcMembers
public class RoomParams: NSObject {
    public var maxSeatCount: Int = 0 //The default value is the maximum number of seat supported by the package
    public var seatMode: TUISeatMode = .applyToTake
    public init(maxSeatCount: Int, seatMode: TUISeatMode) {
        self.maxSeatCount = maxSeatCount
        self.seatMode = seatMode
    }
    public override init() {}
}

@objcMembers
public class TUIVoiceRoomViewController: UIViewController {
    
    @objc public enum RoomBehavior: Int {
        case autoCreate
        case prepareCreate
        case join
    }
    
    public typealias OnStartClosure = () -> Void
    
    // MARK: - Public property.
    public var behavior: RoomBehavior = .prepareCreate
    
    // MARK: - Private property.
   
    private let roomId: String
    private lazy var manager = VoiceRoomManager(provider: self)
    private let routerManager: VRRouterManager = VRRouterManager()
    private var needRestoreNavigationBarHiddenState: Bool = false
    private var cancellableSet = Set<AnyCancellable>()
    private var isShowingRootView = false
    
    private lazy var routerCenter: VRRouterControlCenter = {
        let rootRoute: VRRoute = behavior == .join ? .audience : .anchor
        let routerCenter = VRRouterControlCenter(rootViewController: self, rootRoute: rootRoute, routerManager: routerManager, manager: manager)
        return routerCenter
    }()
    
    private lazy var seatGridView: SeatGridView = {
        func setComponent() {
            do {
                let jsonObject: [String: Any] = [
                    "api": "component",
                    "component": 22
                ]
                let jsonData = try JSONSerialization.data(withJSONObject: jsonObject, options: [])
                if let jsonString = String(data: jsonData, encoding: .utf8) {
                    SeatGridView.callExperimentalAPI(jsonString)
                }
            } catch {
                LiveKitLog.error("\(#file)","\(#line)", "dataReport: \(error.localizedDescription)")
            }
        }
        setComponent()
        return SeatGridView()
    }()
    
    private lazy var voicePrepareView: VoiceRoomPrepareView = {
        let view = VoiceRoomPrepareView(frame: UIScreen.main.bounds,
                                        manager: manager,
                                        routerManager: routerManager)
        view.delegate = self
        return view
    }()
    
    private lazy var voiceRootView: VoiceRoomRootView = {
        let view = VoiceRoomRootView(frame: UIScreen.main.bounds,
                                     roomId: roomId,
                                     seatGridView: seatGridView,
                                     manager: manager,
                                     routerManager: routerManager,
                                     isCreate: behavior != .join)
        view.delegate = self
        return view
    }()
    
    // MARK: - Public function
    public init(roomId: String, behavior: RoomBehavior, roomParams: RoomParams? = nil) {
        self.roomId = roomId
        self.behavior = behavior
        super.init(nibName: nil, bundle: nil)
        subscribeToast()
        manager.prepareRoomIdBeforeEnterRoom(roomId: roomId, roomParams: roomParams)
        UIApplication.shared.isIdleTimerDisabled = true
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        UIApplication.shared.isIdleTimerDisabled = false
        cancellableSet.forEach { $0.cancel() }
        cancellableSet.removeAll()
        StateCache.shared.clear()
        TUIGiftStore.shared.reset()
        print("deinit \(type(of: self))")
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
    
    private func handle(behavior: RoomBehavior) {
        switch behavior {
            case .join, .autoCreate:
                showVoiceRoot()
            case .prepareCreate:
                showVoicePrepare()
        }
        enableSubscribeRouter(enable: true)
    }
    
    private func showVoicePrepare() {
        view.addSubview(voicePrepareView)
        voicePrepareView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
    }
    
    private func showVoiceRoot() {
        view.subviews.forEach{$0.removeFromSuperview()}
        view.addSubview(voiceRootView)
        voiceRootView.snp.makeConstraints { make in
            make.top.bottom.leading.trailing.equalToSuperview()
        }
        isShowingRootView = true
    }
    
    private func showAnchorEndView(info: [String: Any]) {
        view.subviews.forEach {$0.removeFromSuperview()}
        
        // TODO: Transition animation needs to be considered
        
        guard let model = info["data"] as? AnchorEndStatisticsViewInfo else { return }
        let anchorEndView = AnchorEndStatisticsView(endViewInfo: model)
        anchorEndView.delegate = self
        view.addSubview(anchorEndView)
        anchorEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func showAudienceEndView(info: [String: Any]) {
        view.subviews.forEach {$0.removeFromSuperview()}
        
        // TODO: Transition animation needs to be considered
        
        guard let roomId = info["roomId"] as? String else { return }
        guard let avatarUrl = info["avatarUrl"] as? String else { return }
        guard let userName = info["userName"] as? String else { return }
        let audienceEndView = AudienceEndStatisticsView(roomId: roomId, avatarUrl: avatarUrl, userName: userName)
        audienceEndView.delegate = self
        view.addSubview(audienceEndView)
        audienceEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

// MARK: - LiveEndViewDelegate
extension TUIVoiceRoomViewController: AnchorEndStatisticsViewDelegate, AudienceEndStatisticsViewDelegate {
    func onCloseButtonClick() {
        routerManager.router(action: .exit)
    }
}

// MARK: - Store
extension TUIVoiceRoomViewController {
    public func enableSubscribeRouter(enable: Bool) {
        enable ? routerCenter.subscribeRouter() : routerCenter.unSubscribeRouter()
    }
}

// MARK: - Subscribe state
extension TUIVoiceRoomViewController {
    private func subscribeToast() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self else { return }
                view.makeToast(message)
            }.store(in: &cancellableSet)
    }
    
    private func subscribeRouter() {
        routerCenter.subscribeRouter()
    }
}

extension TUIVoiceRoomViewController: VoiceRoomRootViewDelegate {
    func rootView(_ view: VoiceRoomRootView, showEndView endInfo: [String : Any], isAnchor: Bool) {
        isAnchor ? showAnchorEndView(info: endInfo) : showAudienceEndView(info: endInfo)
    }
}

extension TUIVoiceRoomViewController: VoiceRoomPrepareViewDelegate {
    func prepareView(_ view: VoiceRoomPrepareView, didClickStart button: UIButton) {
        showVoiceRoot()
    }
    
    func prepareView(_ view: VoiceRoomPrepareView, didClickBack button: UIButton) {
        routerManager.router(action: .exit)
        if isShowingRootView {
            voiceRootView.onExit()
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

extension TUIVoiceRoomViewController: VoiceRoomManagerProvider {
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        seatGridView.subscribeState(selector)
    }
    
    func getCoreViewState<T>() -> T where T : State {
        seatGridView.getState()
    }
}
