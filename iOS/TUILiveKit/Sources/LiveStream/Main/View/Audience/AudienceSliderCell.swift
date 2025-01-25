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
    func handleScrollToNewRoom(roomId: String, ownerId: String, manager: LiveStreamManager, coreView: LiveCoreView, routerProvider: LSRouterViewProvider, relayoutCoreViewClosure: @escaping () -> Void)
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
    
    private let coreView = LiveCoreView()
    private lazy var manager = LiveStreamManager(provider: self)
    private let routerManager: LSRouterManager
    private lazy var likeManager = LikeManager(roomId: roomId)
    private var cancellableSet = Set<AnyCancellable>()
    
    init(roomId: String, routerManager: LSRouterManager, routerCenter: LSRouterControlCenter, audienceVC: UIViewController & FloatWindowDataSource) {
        self.roomId = roomId
        self.routerManager = routerManager
        self.routerCenter = routerCenter
        self.audienceVC = audienceVC
        super.init(frame: .zero)
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
//        subscribeState()
        isViewReady = true
    }
    
    func onViewWillSlideIn() {
        audienceView.livingView.isHidden = true
        coreView.startPreloadLiveStream(roomId: roomId, isMuteAudio: true)
    }

    func onViewDidSlideIn() {
        manager.initSelfUserData()
        enterRoom()
    }
    
    func onViewSlideInCancelled() {
    }
    
    func onViewWillSlideOut() {
    }
    
    func onViewDidSlideOut() {
        if !FloatWindow.shared.isShowingFloatWindow() {
            coreView.stopPreloadLiveStream(roomId: roomId)
            coreView.leaveLiveStream() { [weak self] in
                self?.manager.resetAllState()
            } onError: { _, _ in
            }
        }
    }
    
    func onViewSlideOutCancelled() {
    }
    
    func enterRoom() {
        delegate?.handleScrollToNewRoom(roomId: roomId, ownerId: manager.coreRoomState.ownerInfo.userId,
                                        manager: manager, coreView: coreView,
                                        routerProvider: audienceView) { [weak self] in
            guard let self = self else { return }
            audienceView.relayoutCoreView()
        }
        audienceView.joinLiveStream()
    }
    
    deinit {
        debugPrint("deinit:\(self)")
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
        manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
            .removeDuplicates()
            .receive(on: RunLoop.main)
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
