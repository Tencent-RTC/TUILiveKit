//
//  SeatGridView.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//

import UIKit
import Combine
import SnapKit
import RTCRoomEngine
import RTCCommon
import TUICore

@objcMembers
public class SeatGridView: UIView {
    // MARK: - public property.
    public weak var delegate: SGSeatViewDelegate?
    
    public init() {
        super.init(frame: .zero)
        SGDataReporter.reportEventData(event: .panelShowSeatGridView)
    }
    
    public required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        SGDataReporter.reportEventData(event: .panelHideSeatGridView)
        debugPrint("deinit:\(self)")
    }
    
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        initializeManagerData()
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        isViewReady = true
    }
    
    // MARK: - Private store property.
    private var manager = SeatGridViewManager()
    private var cancellableSet: Set<AnyCancellable> = []
    private var isViewReady = false
    
    // MARK: - Private calculate property.
  
    private var roomState: SGRoomState {
        manager.roomState
    }
    
    private lazy var seatContainerView: UICollectionView = {
        let layoutConfig = SGSeatViewLayoutConfig()
        let layout = SeatGridViewLayout(rowSpacing: layoutConfig.rowSpacing, rowConfigs: layoutConfig.rowConfigs)
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.backgroundColor = .clear
        view.register(SGSeatContainerCell.self, forCellWithReuseIdentifier: SGSeatContainerCell.identifier)
        view.delegate = self
        view.dataSource = self
        return view
    }()
    
}

// MARK: - public API
extension SeatGridView {
    public func addObserver(observer: SeatGridViewObserver) {
        manager.addObserver(observer)
    }
    
    public func removeObserver(observer: SeatGridViewObserver) {
        manager.removerObserver(observer)
    }
    
    public func setLayoutMode(layoutMode: SGLayoutMode, layoutConfig: SGSeatViewLayoutConfig? = nil) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewSetLayoutMode)
        manager.setLayoutMode(layoutMode: layoutMode, layoutConfig: layoutConfig)
    }
    
    public func setSeatViewDelegate(_ delegate: SGSeatViewDelegate) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewSetSeatViewDelegate)
        self.delegate = delegate
    }
}

extension SeatGridView {
    public func startVoiceRoom(roomInfo: TUIRoomInfo, onSuccess: @escaping SGOnRoomSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStartRoom)
        Task {
            do {
                let roomInfo = try await manager.create(roomInfo: roomInfo)
                let _ = try await manager.takeSeat(index: -1, timeout: kSGDefaultTimeout)
                runOnMainThread { onSuccess(roomInfo) }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func stopVoiceRoom(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStopRoom)
        Task {
            do {
                try await manager.destroy()
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func joinVoiceRoom(roomId: String, onSuccess: @escaping SGOnRoomSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewJoinRoom)
        Task {
            do {
                let roomInfo = try await manager.enter(roomId: roomId)
                runOnMainThread { onSuccess(roomInfo) }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func leaveVoiceRoom(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLeaveRoom)
        Task {
            do {
                try await manager.leave()
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func updateRoomSeatMode(seatMode: TUISeatMode, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewUpdateSeatMode)
        Task {
            do {
                try await manager.update(seatMode: seatMode)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
}

extension SeatGridView {
    public func takeSeat(index: Int,
                  timeout: Int,
                  onAccepted: @escaping SGOnRequestAccepted,
                  onRejected: @escaping SGOnRequestRejected,
                  onCancelled: @escaping SGOnRequestCancelled,
                  onTimeout: @escaping SGOnRequestTimeout,
                  onError: @escaping SGOnRequestError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewTakeSeat)
        Task {
            do {
                let result = try await manager.takeSeat(index: index, timeout: timeout)
                switch result {
                    case .accepted(let userInfo):
                        runOnMainThread { onAccepted(userInfo) }
                    case .rejected(let userInfo):
                        runOnMainThread { onRejected(userInfo) }
                    case .timeout(let userInfo):
                        runOnMainThread { onTimeout(userInfo) }
                    case .cancel(let userInfo):
                        runOnMainThread { onCancelled(userInfo) }
                }
            } catch let SeatGridViewError.seatErrorWithUser(userInfo, code, message) {
                    runOnMainThread { onError(userInfo, code, message) }
            }
        }
    }
    
    public func cancelRequest(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewCancelRequest)
        Task {
            do {
                try await manager.cancelRequest(userId: userId)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func responseRemoteRequest(userId: String, agree: Bool, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewResponseRequest)
        Task {
            do {
                try await manager.responseRemoteRequest(userId: userId, agree: agree)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func moveToSeat(index: Int, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewMoveToSeat)
        Task {
            do {
                try await manager.moveToSeat(index: index)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func leaveSeat(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLeaveSeat)
        Task {
            do {
                try await manager.leaveSeat()
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func lockSeat(index: Int, lockMode: TUISeatLockParams, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLockSeat)
        Task {
            do {
                try await manager.lockSeat(index: index, lockMode: lockMode)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func takeUserOnSeatByAdmin(index: Int,
                                      timeout: Int,
                                      userId: String,
                                      onAccepted: @escaping SGOnRequestAccepted,
                                      onRejected: @escaping SGOnRequestRejected,
                                      onCancelled: @escaping SGOnRequestCancelled,
                                      onTimeout: @escaping SGOnRequestTimeout,
                                      onError: @escaping SGOnRequestError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewTakeUserOnSeat)
        Task {
            do {
                let result = try await manager.takeUserOnSeatByAdmin(index: index,
                                                                     userId: userId,
                                                                     timeout: timeout)
                switch result {
                    case .accepted(let userInfo):
                        runOnMainThread { onAccepted(userInfo) }
                    case .rejected(let userInfo):
                        runOnMainThread { onRejected(userInfo) }
                    case .timeout(let userInfo):
                        runOnMainThread { onTimeout(userInfo) }
                    case .cancel(let userInfo):
                        runOnMainThread { onCancelled(userInfo) }
                }
            }  catch let SeatGridViewError.seatErrorWithUser(userInfo, code, message) {
                    runOnMainThread { onError(userInfo, code, message) }
            }
        }
    }
    
    public func kickUserOffSeatByAdmin(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewKickUserOffSeat)
        Task {
            do {
                try await manager.kickUserOffSeatByAdmin(userId: userId)
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
}

extension SeatGridView {
    public func startMicrophone(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStartMicrophone)
        Task {
            do {
                try await manager.startMicrophone()
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
    
    public func stopMicrophone() {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStopMicrophone)
        manager.stopMicrophone()
    }
    
    public func muteMicrophone() {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewMuteMicrophone)
        manager.muteMicrophone()
    }
    
    public func unmuteMicrophone(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewUnmuteMicrophone)
        Task {
            do {
                try await manager.unmuteMicrophone()
                runOnMainThread { onSuccess() }
            } catch let SeatGridViewError.error(code, message) {
                runOnMainThread { onError(code, message) }
            }
        }
    }
}

// MARK: - UICollectionViewDataSource & UICollectionViewDelegate
extension SeatGridView: UICollectionViewDataSource, UICollectionViewDelegate {
    public func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
    
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return manager.seatState.seatList.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: SGSeatContainerCell.identifier, for: indexPath)
        if let cell = cell as? SGSeatContainerCell {
            configureSeatView(view: cell, at: indexPath)
            bindSeatViewClosure(view: cell)
            bindSeatViewState(view: cell, at: indexPath)
        }
        return cell
    }
    
    public func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard let cell = collectionView.cellForItem(at: indexPath) as? SGSeatContainerCell, let seatView =  cell.contentContainerView.subviews.first else { return }
        let seatInfo = manager.seatState.seatList[indexPath.row]
        manager.notifyObserverEvent { observer in
            observer.onSeatViewClicked(seatView: seatView, seatInfo: seatInfo)
        }
    }
    
    private func configureSeatView(view: SGSeatContainerCell, at indexPath: IndexPath) {
        let seatInfo = manager.seatState.seatList[indexPath.row]
        let customView = self.delegate?.seatGridView(self, createSeatView: seatInfo)
        let ownerId = manager.roomState.ownerId
        view.configure(with: SeatContainerCellModel(customView: customView, seatInfo: seatInfo, ownerId: ownerId))
    }
    
    private func bindSeatViewClosure(view: SGSeatContainerCell) {
        view.volumeClosure = { [weak self] volume, customView in
            guard let self = self else { return }
            self.delegate?.seatGridView(self, updateUserVolume: volume, seatView: customView)
        }
        view.seatInfoClosure = { [weak self] seatInfo, customView in
            guard let self = self else { return }
            self.delegate?.seatGridView(self, updateSeatView: seatInfo, seatView: customView)
        }
    }
    
    private func bindSeatViewState(view: SGSeatContainerCell, at indexPath: IndexPath) {
        let seatInfoPublisher = manager.subscribeSeatState(StateSelector(keyPath: \SGSeatState.seatList))
            .map { seatList in
                guard indexPath.row < seatList.count else { return TUISeatInfo() }
                return seatList[indexPath.row]
            }.eraseToAnyPublisher()
        seatInfoPublisher
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak view] seatInfo in
                guard let view = view else { return }
                view.seatInfo = seatInfo
            })
            .store(in: &view.cancellableSet)
        manager.subscribeUserState(StateSelector(keyPath: \SGUserState.speakingUserList))
            .removeDuplicates()
            .combineLatest(seatInfoPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak view] users, seatInfo in
                guard let seatView = view else { return }
                if let userId = seatInfo.userId, !userId.isEmpty {
                    if let speakingUser = users.first(where: { $0.userId == userId }) {
                        seatView.isSpeaking = true
                        seatView.volume = speakingUser.volume
                    } else {
                        seatView.isSpeaking = false
                    }
                }
            }
            .store(in: &view.cancellableSet)
        manager.subscribeUserState(StateSelector(keyPath: \SGUserState.hasAudioStreamUserList))
            .removeDuplicates()
            .combineLatest(seatInfoPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak view] users, seatInfo in
                guard let seatView = view else { return }
                if let userId = seatInfo.userId, !userId.isEmpty {
                    seatView.isAudioMuted = !users.contains(userId)
                }
            }
            .store(in: &view.cancellableSet)
    }
}

// MARK: - Private func.
private extension SeatGridView {
    
    private func runOnMainThread(_ closure: @escaping () -> Void) {
        DispatchQueue.main.async {
            closure()
        }
    }
    
    private func constructViewHierarchy() {
        addSubview(seatContainerView)
    }
    
    private func activateConstraints() {
        seatContainerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func initializeManagerData() {
        manager.refreshSelfInfo()
    }
    
    func subscribeState() {
        subscribeViewState()
        subscribeSeatState()
    }
    
    private func subscribeViewState() {
        manager.subscribeViewState(StateSelector(projector: {  $0 }))
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                self.updateSeatGridLayout(layoutConfig: state.layoutConfig)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatState() {
        let seatListSelector = StateSelector(keyPath: \SGSeatState.seatList)
        manager.subscribeSeatState(seatListSelector)
            .map { $0.count }
            .filter { $0 != 0 }
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.seatContainerView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    private func updateSeatGridLayout(layoutConfig: SGSeatViewLayoutConfig) {
        let layout = SeatGridViewLayout(rowSpacing: layoutConfig.rowSpacing, rowConfigs: layoutConfig.rowConfigs)
        seatContainerView.collectionViewLayout.invalidateLayout()
        seatContainerView.setCollectionViewLayout(layout, animated: true)
    }
}
