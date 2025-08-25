//
//  AudienceSliderCell.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/12/30.
//

import LiveStreamCore
import RTCRoomEngine
import RTCCommon
import Combine

protocol AudienceListCellDelegate: AnyObject {
    func handleScrollToNewRoom(roomId: String, ownerId: String, manager: AudienceManager, coreView: LiveCoreView, relayoutCoreViewClosure: @escaping () -> Void)
    func showFloatWindow()
    func showToast(message: String)
    func disableScrolling()
    func enableScrolling()
    func scrollToNextPage()
    func onRoomDismissed(roomId: String, avatarUrl: String, userName: String)
}

class AudienceSliderCell: UIView {
    weak var delegate: AudienceListCellDelegate?
    weak var rotateScreenDelegate: RotateScreenDelegate?

    private var roomId: String
    private var isViewReady = false
    private var isCurrentShowCell = false
    private weak var routerCenter: AudienceRouterControlCenter?
    
    private lazy var coreView: LiveCoreView = {
        func setComponent() {
            do {
                let jsonObject: [String: Any] = [
                    "api": "component",
                    "component": 21
                ]
                let jsonData = try JSONSerialization.data(withJSONObject: jsonObject, options: [])
                if let jsonString = String(data: jsonData, encoding: .utf8) {
                    LiveCoreView.callExperimentalAPI(jsonString)
                }
            } catch {
                LiveKitLog.error("\(#file)","\(#line)", "dataReport: \(error.localizedDescription)")
            }
        }
        setComponent()
        return LiveCoreView()
    }()

    private lazy var manager = AudienceManager(provider: self)
    private let routerManager: AudienceRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    private var isStartedPreload = false
    
    init(liveInfo: LiveInfo, routerManager: AudienceRouterManager, routerCenter: AudienceRouterControlCenter) {
        self.roomId = liveInfo.roomId
        self.routerManager = routerManager
        self.routerCenter = routerCenter
        super.init(frame: .zero)
        manager.onAudienceSliderCellInit(liveInfo: liveInfo)
        debugPrint("init:\(self)")
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var audienceView: AudienceView = {
        let view = AudienceView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: coreView)
        view.rotateScreenDelegate = self
        return view
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        subscribeSubjects()
        subscribeState()
        subscribeRoomState()
        isViewReady = true
    }
    
    func onViewWillSlideIn() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewWillSlideIn roomId: \(roomId)")
        audienceView.livingView.isHidden = true
        coreView.startPreviewLiveStream(roomId: roomId, isMuteAudio: true)
        isStartedPreload = true
    }

    func onViewDidSlideIn() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewDidSlideIn roomId: \(roomId)")
        enterRoom()
        isCurrentShowCell = true
    }
    
    func onViewSlideInCancelled() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewSlideInCancelled roomId: \(roomId)")
        coreView.stopPreviewLiveStream(roomId: roomId)
    }
    
    func onViewWillSlideOut() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewWillSlideOut roomId: \(roomId)")
    }
    
    func onViewDidSlideOut() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewDidSlideOut roomId: \(roomId)")
        if !FloatWindow.shared.isShowingFloatWindow() {
            coreView.stopPreviewLiveStream(roomId: roomId)
            coreView.leaveLiveStream() { [weak self] in
                guard let self = self else { return }
                manager.onLeaveLive()
            } onError: { _, _ in
            }
            isStartedPreload = false
        }
        isCurrentShowCell = false
    }
    
    func onViewSlideOutCancelled() {
        LiveKitLog.info("\(#file)","\(#line)", "onViewSlideOutCancelled roomId: \(roomId)")
    }
    
    func enterRoom() {
        delegate?.handleScrollToNewRoom(roomId: roomId, ownerId: manager.coreRoomState.ownerInfo.userId,
                                        manager: manager, coreView: coreView) { [weak self] in
            guard let self = self else { return }
            audienceView.relayoutCoreView()
        }
        delegate?.disableScrolling()
        audienceView.joinLiveStream() { [weak self] result in
            guard let self = self else { return }
            if case .success = result {
                delegate?.enableScrolling()
            }
        }
    }
    
    deinit {
        debugPrint("deinit:\(self)")
        if isStartedPreload {
            coreView.stopPreviewLiveStream(roomId: roomId)
        }
    }
}

// MARK: - private func
extension AudienceSliderCell {
    private func constructViewHierarchy() {
        addSubview(audienceView)
    }
    
    private func activateConstraints() {
        audienceView.snp.makeConstraints{ make in
            make.edges.equalToSuperview()
        }
    }
    
    private func subscribeSubjects() {
        manager.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] message in
                guard let self = self, let delegate = delegate else { return }
                delegate.showToast(message: message)
            }.store(in: &cancellableSet)
        
        manager.floatWindowSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self, let delegate = delegate else { return }
                delegate.showFloatWindow()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.coGuestStatus))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .dropFirst()
            .sink { [weak self] coGuestStatus in
                guard let self = self, let delegate = delegate else { return }
                if coGuestStatus == .applying || coGuestStatus == .linking {
                    delegate.disableScrolling()
                } else {
                    delegate.enableScrolling()
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeState(StateSelector(keyPath: \AudienceRoomState.liveStatus))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .finished:
                    let avaUrl = manager.roomState.liveInfo.ownerAvatarUrl
                    let userName = manager.roomState.liveInfo.ownerName
                    delegate?.onRoomDismissed(roomId: manager.roomState.roomId, avatarUrl: avaUrl, userName: userName)
                    default: break
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceRoomState.roomVideoStreamIsLandscape))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] videoStreamIsLandscape in
                guard let self = self else { return }
                if !videoStreamIsLandscape && isCurrentShowCell {
                    self.rotateScreen(isPortrait: true)
                }
            }
            .store(in: &cancellableSet)
    }
}

extension AudienceSliderCell: AudienceManagerProvider {
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        coreView.subscribeState(selector)
    }
    
    func getCoreViewState<T: State>() -> T {
        coreView.getState()
    }
}

extension AudienceSliderCell: RotateScreenDelegate {
    func rotateScreen(isPortrait: Bool) {
        rotateScreenDelegate?.rotateScreen(isPortrait: isPortrait)
    }
}
