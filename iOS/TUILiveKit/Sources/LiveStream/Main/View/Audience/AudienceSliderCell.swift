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
import TUILiveComponent

protocol AudienceListCellDelegate: AnyObject {
    func handleScrollToNewRoom(roomId: String, ownerId: String, manager: LiveStreamManager, coreView: LiveCoreView, relayoutCoreViewClosure: @escaping () -> Void)
    func showFloatWindow()
    func showToast(message: String)
    func disableScrolling()
    func enableScrolling()
    func scrollToNextPage()
}

class AudienceSliderCell: UIView {
    weak var delegate: AudienceListCellDelegate?
    
    private var roomId: String
    private var isViewReady = false
    private weak var routerCenter: LSRouterControlCenter?
    private weak var audienceVC: (UIViewController & FloatWindowDataSource)?
    
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
    private lazy var manager = LiveStreamManager(provider: self)
    private let routerManager: LSRouterManager
    private lazy var likeManager = LikeManager(roomId: roomId)
    private var cancellableSet = Set<AnyCancellable>()
    private var isStartedPreload = false
    private var isEnteredRoom = false
    
    init(liveInfo: LiveInfo, routerManager: LSRouterManager, routerCenter: LSRouterControlCenter, audienceVC: UIViewController & FloatWindowDataSource) {
        self.roomId = liveInfo.roomId
        self.routerManager = routerManager
        self.routerCenter = routerCenter
        self.audienceVC = audienceVC
        super.init(frame: .zero)
        manager.onAudienceSliderCellInit(liveInfo: liveInfo)
        debugPrint("init:\(self)")
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var audienceView: AudienceView = {
        let view = AudienceView(roomId: roomId, manager: manager, routerManager: routerManager, coreView: coreView)
        return view
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        subscribeSubjects()
        subscribeState()
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
            isEnteredRoom = false
        }
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
        audienceView.joinLiveStream() { [weak self] in
            guard let self = self else { return }
            delegate?.enableScrolling()
        }
        isEnteredRoom = true
    }
    
    deinit {
        debugPrint("deinit:\(self)")
        if isStartedPreload {
            coreView.stopPreviewLiveStream(roomId: roomId)
        }
        if isEnteredRoom {
            coreView.leaveLiveStream() {
            } onError: { _, _ in
            }
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
        
        manager.likeSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                likeManager.sendLike()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeState() {
        manager.subscribeState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
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
    }
}

extension AudienceSliderCell: LiveStreamManagerProvider {
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        coreView.subscribeState(selector)
    }
    
    func getCoreViewState<T: State>() -> T {
        coreView.getState()
    }
}
