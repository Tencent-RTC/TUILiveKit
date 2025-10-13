//
//  SeatGridView.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//

import UIKit
import Combine
import RTCRoomEngine
import RTCCommon
import AtomicXCore

private struct RequestCallback {
    let onAccepted: SGOnRequestAccepted
    let onRejected: SGOnRequestRejected
    let onCancelled: SGOnRequestCancelled
    let onTimeout: SGOnRequestTimeout
    let onError: SGOnRequestError
}

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
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        isViewReady = true
    }
    
    // MARK: - Private store property.
    private var observers: SGSeatGridObserverList = SGSeatGridObserverList()
    private var cancellableSet: Set<AnyCancellable> = []
    private var isViewReady = false
    private var liveId = ""
    
    private var applyCallback: RequestCallback?
    private var inviteCallbacks: [String: RequestCallback] = [:]
    
    private var invitation: LiveUserInfo?
    
    // MARK: - Private calculate property.
    
    private(set) lazy var viewManager = SGViewManager(provider: self)
    
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
    
    private var selfInfo: TUIUserInfo {
        let selfId = TUIRoomEngine.getSelfInfo().userId
        return TUIUserInfo(userId: selfId)
    }
    
    private var isSelfOwner: Bool {
        let selfId = TUIRoomEngine.getSelfInfo().userId
        let ownerId = liveListStore.state.value.currentLive.liveOwner.userID
        if selfId.isEmpty || ownerId.isEmpty {
            return false
        }
        return selfId == ownerId
    }
}

// MARK: - public API
extension SeatGridView {
    public func setLiveId(_ liveId: String) {
        self.liveId = liveId
    }
    
    public func setLayoutMode(layoutMode: SGLayoutMode, layoutConfig: SGSeatViewLayoutConfig? = nil) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewSetLayoutMode)
        viewManager.setLayoutMode(layoutMode: layoutMode, layoutConfig: layoutConfig)
    }
    
    public func setSeatViewDelegate(_ delegate: SGSeatViewDelegate) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewSetSeatViewDelegate)
        self.delegate = delegate
    }
    
    public static func callExperimentalAPI(_ jsonStr: String) {
        do {
            guard let data = jsonStr.data(using: .utf8) else {
                throw NSError(domain: "InvalidJSON", code: -1, userInfo: nil)
            }
            
            let jsonObject = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
            
            if let api = jsonObject?["api"] as? String, api == "component",
               let component = jsonObject?["component"] as? Int {
                switch component {
                case Constants.DataReport.componentLiveRoom:
                    Constants.component = Constants.DataReport.componentLiveRoom
                case Constants.DataReport.componentVoiceRoom:
                    Constants.component = Constants.DataReport.componentVoiceRoom
                default:
                    Constants.component = Constants.DataReport.componentCoreView
                }
                return
            }
            
            if let api = jsonObject?["api"] as? String, api == "setAvatarPlaceholderImage" {
                SeatGridViewLog.info("API setAvatarPlaceholderImage jsonString:\(jsonStr)")
                guard let paramsDic = jsonObject?["params"] as? [String : Any] else {
                    return
                }
                if let imagePath = paramsDic["imagePath"] as? String {
                    SGResourceConfig.avatarPlaceholderImage = imagePath
                }
                return
            }
            if let api = jsonObject?["api"] as? String, api == "setHttpHeaderField" {
                SeatGridViewLog.info("API setHttpHeaderField jsonString:\(jsonStr)")
                guard let paramsDic = jsonObject?["params"] as? [String : Any] else {
                    return
                }
                if let httpHeaderFieldKey = paramsDic["httpHeaderFieldKey"] as? String {
                    SGResourceConfig.httpHeaderFieldKey = httpHeaderFieldKey
                }
                if let httpHeaderFieldValue = paramsDic["httpHeaderFieldValue"] as? String {
                    SGResourceConfig.httpHeaderFieldValue = httpHeaderFieldValue
                }
                return
            }
        } catch let error {
            VRLog.error("callExperimentalAPI \(error.localizedDescription)")
        }
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonStr) { message in
        }
    }
}

// MARK: - deprecated public API
@available(*, deprecated)
extension SeatGridView {
    public func addObserver(observer: SeatGridViewObserver) {
        observers.addObserver(observer)
    }
    
    public func removeObserver(observer: SeatGridViewObserver) {
        observers.removeObserver(observer)
    }
    
    public func startVoiceRoom(liveInfo: TUILiveInfo, onSuccess: @escaping TUILiveInfoBlock, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStartRoom)
        
        let liveInfo = AtomicXCore.LiveInfo.init(from: liveInfo)
        liveListStore.createLive(liveInfo) { [weak self] result in
            guard let self = self else { return }
            
            switch result {
            case .success(let liveInfo):
                let tuiLiveInfo = TUILiveInfo.init(from: liveInfo)
                notifyEnterRoom()
                onSuccess(tuiLiveInfo)
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }

    public func stopVoiceRoom(onSuccess: @escaping TUIStopLiveBlock, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStopRoom)
        
        liveListStore.endLive { [weak self] result in
            guard let self = self else { return }
            
            switch result {
            case .success(let data):
                notifyExitRoom()
                onSuccess(data)
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }

    public func joinVoiceRoom(roomId: String, onSuccess: @escaping TUILiveInfoBlock, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewJoinRoom)

        liveListStore.joinLive(liveID: roomId) { [weak self] result in
            guard let self = self else { return }
            
            switch result {
            case .success(let liveInfo):
                let tuiLiveInfo = TUILiveInfo.init(from: liveInfo)
                notifyEnterRoom()
                onSuccess(tuiLiveInfo)
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }

    public func leaveVoiceRoom(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLeaveRoom)

        liveListStore.leaveLive {[weak self] result in
            guard let self = self else { return }
            
            switch result {
            case .success():
                notifyExitRoom()
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
    
    public func updateRoomSeatMode(seatMode: TUISeatMode, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewUpdateSeatMode)
        
        var currentLive = liveListStore.state.value.currentLive
        guard !currentLive.isEmpty else {
            onError(TUIError.failed.rawValue, "Not in room")
            return
        }
        currentLive.seatMode = TakeSeatMode.init(from: seatMode)
        let modifyFlag = AtomicLiveInfo.ModifyFlag.seatMode
        liveListStore.updateLiveInfo(currentLive, modifyFlag: modifyFlag) { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
}

@available(*, deprecated)
extension SeatGridView {
    public func takeSeat(index: Int,
                  timeout: Int,
                  onAccepted: @escaping SGOnRequestAccepted,
                  onRejected: @escaping SGOnRequestRejected,
                  onCancelled: @escaping SGOnRequestCancelled,
                  onTimeout: @escaping SGOnRequestTimeout,
                  onError: @escaping SGOnRequestError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewTakeSeat)
        
        if isAutoOnSeat() {
            seatStore.takeSeat(seatIndex: index) { [weak self] result in
                guard let self = self else { return }
                
                switch result {
                case .success():
                    onAccepted(selfInfo)
                case .failure(let error):
                    onError(selfInfo, error.code, error.message)
                }
            }
            return
        }
        
        applyCallback = RequestCallback(onAccepted: onAccepted,
                                        onRejected: onRejected,
                                        onCancelled: onCancelled,
                                        onTimeout: onTimeout,
                                        onError: onError)
        coGuestStore.applyForSeat(seatIndex: index,
                                  timeout: TimeInterval(timeout),
                                  extraInfo: nil) { [weak self] result in
            guard let self = self else { return }

            switch result {
            case .failure(let error):
                onError(selfInfo, error.code, error.message)
            default: break
            }
        }
    }
    
    public func cancelRequest(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewCancelRequest)
        
        if !isSelfOwner {
            coGuestStore.cancelApplication { [weak self] result in
                guard let self = self else { return }
                
                switch result {
                case .success():
                    applyCallback?.onCancelled(selfInfo)
                    applyCallback = nil
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
        } else {
            coGuestStore.cancelInvitation(inviteeID: userId) { [weak self] result in
                guard let self = self else { return }
            
                switch result {
                case .success():
                    inviteCallbacks[userId]?.onCancelled(selfInfo)
                    inviteCallbacks.removeValue(forKey: userId)
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
        }
    }
    
    public func responseRemoteRequest(userId: String, agree: Bool, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewResponseRequest)
        
        if agree {
            acceptRemoteRequest(userId: userId, onSuccess: onSuccess, onError: onError)
        } else {
            rejectRemoteRequest(userId: userId, onSuccess: onSuccess, onError: onError)
        }
    }
    
    public func moveToSeat(index: Int, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewMoveToSeat)
        
        seatStore.moveUserToSeat(userID: selfInfo.userId, targetIndex: index, policy: .abortWhenOccupied) { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
    
    public func leaveSeat(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLeaveSeat)
        
        coGuestStore.disConnect { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
        
    }
    
    public func lockSeat(index: Int, lockMode: TUISeatLockParams, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewLockSeat)
        // TOOD: 暂时调用engine接口，之后需要调整接口和seatStore形式保持一致
        TUIRoomEngine.sharedInstance().lockSeatByAdmin(index, lockMode: lockMode) {
            onSuccess()
        } onError: { error, message in
            onError(error.rawValue, message)
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
        
        inviteCallbacks[userId] = RequestCallback(onAccepted: onAccepted,
                                                  onRejected: onRejected,
                                                  onCancelled: onCancelled,
                                                  onTimeout: onTimeout,
                                                  onError: onError)
        
        coGuestStore.inviteToSeat(userID: userId,
                                  seatIndex: index,
                                  timeout: TimeInterval(timeout),
                                  extraInfo: nil) { [weak self] result in
            guard let self = self else { return }

            switch result {
            case .failure(let error):
                onError(selfInfo, error.code, error.message)
            default: break
            }
        }
    }
    
    public func kickUserOffSeatByAdmin(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewKickUserOffSeat)
        
        seatStore.kickUserOutOfSeat(userID: userId) { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
}

@available(*, deprecated)
extension SeatGridView {
    public func startMicrophone(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStartMicrophone)
        
        deviceStore.openLocalMicrophone { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
    
    public func stopMicrophone() {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewStopMicrophone)
       
        deviceStore.closeLocalMicrophone()
    }
    
    public func muteMicrophone() {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewMuteMicrophone)
       
        seatStore.muteMicrophone()
    }
    
    public func unmuteMicrophone(onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        SGDataReporter.reportEventData(event: .methodCallSeatGridViewUnmuteMicrophone)

        seatStore.unmuteMicrophone { result in
            switch result {
            case .success():
                onSuccess()
            case .failure(let error):
                onError(error.code, error.message)
            }
        }
    }
}

extension SeatGridView: SGViewManagerDataProvider {
    var deviceStore: DeviceStore {
        return DeviceStore.shared
    }
    
    var liveListStore: LiveListStore {
        return LiveListStore.shared
    }
    
    var coGuestStore: CoGuestStore {
        return CoGuestStore.create(liveID: liveId)
    }
    
    var seatStore: LiveSeatStore {
        return LiveSeatStore.create(liveID: liveId)
    }
    
    var seatListCount: Int {
        return seatStore.state.value.seatList.count
    }
}

extension SeatGridView {
    private func setupliveEventListener() {
        liveListStore.liveListEventPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] event in
                guard let self = self else { return }
                
                switch event {
                case .onLiveEnded(liveID: let liveId, reason: _, message: _):
                    guard self.liveId == liveId else { return }
                    notifyObserverEvent { observer in
                        observer.onRoomDismissed(roomId: liveId)
                    }
                case  .onKickedOutOfLive(liveID: let liveId, reason: let reason, message: let message):
                    notifyObserverEvent { observer in
                        observer.onKickedOutOfRoom(roomId: liveId, reason: .init(from: reason), message: message)
                    }
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func setupGuestEventListener() {
        coGuestStore.guestEventPublisher
            .receive(on: RunLoop.main)
             .sink { [weak self] event in
                 guard let self = self else { return }
                 
                 switch event {
                 case .onHostInvitationReceived(hostUser: let hostUser):
                     invitation = hostUser
                     notifyObserverEvent{ observer in
                         observer.onSeatRequestReceived(type: .inviteToTakeSeat, userInfo: TUIUserInfo(from: hostUser))
                     }
                 case .onHostInvitationCancelled(hostUser: let hostUser):
                     invitation = nil
                     notifyObserverEvent { observer in
                         observer.onSeatRequestCancelled(type: .inviteToTakeSeat, userInfo: TUIUserInfo(from: hostUser))
                     }
                 case .onGuestApplicationResponded(isAccept: let isAccept, hostUser: let handleUser):
                     if isAccept {
                         applyCallback?.onAccepted(.init(from: handleUser))
                     } else {
                         applyCallback?.onRejected(.init(from: handleUser))
                     }
                     applyCallback = nil
                 case .onGuestApplicationNoResponse(reason: let reason):
                     if reason == .timeout {
                         applyCallback?.onTimeout(TUIUserInfo(userId: selfInfo.userId))
                     }
                     applyCallback = nil
                 case .onKickedOffSeat(seatIndex: _, hostUser: let handleUser):
                     notifyObserverEvent { observer in
                         observer.onKickedOffSeat(userInfo: TUIUserInfo(from: handleUser))
                     }
                 }
             }
             .store(in: &cancellableSet)
     }
    
    private func setupHostEventListener() {
        coGuestStore.hostEventPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] event in
                guard let self = self else { return }
                
                switch event {
                case .onGuestApplicationReceived(guestUser: let guestUser):
                    notifyObserverEvent { observer in
                        observer.onSeatRequestReceived(type: .applyToTakeSeat, userInfo: TUIUserInfo(from: guestUser))
                    }
                case .onGuestApplicationCancelled(guestUser: let guestUser):
                    notifyObserverEvent { observer in
                        observer.onSeatRequestCancelled(type: .applyToTakeSeat, userInfo: TUIUserInfo(from: guestUser))
                    }
                case .onGuestApplicationProcessedByOtherHost(guestUser: _, hostUser: _):
                    break
                case .onHostInvitationResponded(isAccept: let isAccept, guestUser: let guestUser):
                    if isAccept {
                        inviteCallbacks[guestUser.userID]?.onAccepted(.init(from: guestUser))
                    } else {
                        inviteCallbacks[guestUser.userID]?.onRejected(.init(from: guestUser))
                    }
                    inviteCallbacks.removeValue(forKey: guestUser.userID)
                case .onHostInvitationNoResponse(guestUser: let guestUser, reason: let reason):
                    if reason == .timeout {
                        inviteCallbacks[guestUser.userID]?.onTimeout(.init(from: guestUser))
                    }
                    inviteCallbacks.removeValue(forKey: guestUser.userID)
                }
            }
            .store(in: &cancellableSet)
    }
    
    func acceptRemoteRequest(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        if let receivedInvitation = invitation, receivedInvitation.userID == userId {
            coGuestStore.acceptInvitation(inviterID: userId) { [weak self] result in
                guard let self = self else { return }
                
                switch result {
                case .success():
                    invitation = nil
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
            return
        }
        
        if coGuestStore.state.value.applicants.contains(where: { $0.userID == userId }) {
            coGuestStore.acceptApplication(userID: userId) { result in
                switch result {
                case .success():
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
            return
        }
    }
    
    func rejectRemoteRequest(userId: String, onSuccess: @escaping SGOnSuccess, onError: @escaping SGOnError) {
        if let receivedInvitation = invitation, receivedInvitation.userID == userId {
            coGuestStore.rejectInvitation(inviterID: userId) { [weak self] result in
                guard let self = self else { return }
                
                switch result {
                case .success():
                    invitation = nil
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
            return
        }
        
        if coGuestStore.state.value.applicants.contains(where: { $0.userID == userId }) {
            coGuestStore.rejectApplication(userID: userId) { result in
                switch result {
                case .success():
                    onSuccess()
                case .failure(let error):
                    onError(error.code, error.message)
                }
            }
            return
        }
    }
}

// MARK: - UICollectionViewDataSource & UICollectionViewDelegate
extension SeatGridView: UICollectionViewDataSource, UICollectionViewDelegate {
    public func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
    
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return seatStore.state.value.seatList.count
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
        let seatInfo = seatStore.state.value.seatList[indexPath.row]
        notifyObserverEvent { observer in
            observer.onSeatViewClicked(seatView: seatView, seatInfo: TUISeatInfo(from: seatInfo))
        }
    }
    
    private func configureSeatView(view: SGSeatContainerCell, at indexPath: IndexPath) {
        let seatInfo = seatStore.state.value.seatList[indexPath.row]
        let tuiSeatInfo =  TUISeatInfo(from: seatInfo)
        let customView = self.delegate?.seatGridView(self, createSeatView: tuiSeatInfo)
        let ownerId = liveListStore.state.value.currentLive.liveOwner.userID
        view.configure(with: SeatContainerCellModel(customView: customView, seatInfo: tuiSeatInfo, ownerId: ownerId))
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
        let seatInfoPublisher = seatStore.state.subscribe(StatePublisherSelector(keyPath: \LiveSeatState.seatList))
            .compactMap { seatList -> SeatInfo? in
                guard indexPath.row < seatList.count else { return nil }
                let seatInfo = seatList[indexPath.row]
                return seatInfo
            }
            .eraseToAnyPublisher()
        
        seatInfoPublisher
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak view] seatInfo in
                guard let view = view else { return }
                view.seatInfo = TUISeatInfo(from: seatInfo)
            })
            .store(in: &view.cancellableSet)
        seatStore.state.subscribe(StatePublisherSelector(keyPath: \LiveSeatState.speakingUsers))
            .removeDuplicates()
            .combineLatest(seatInfoPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak view] userVolumeMap, seatInfo in
                guard let seatView = view else { return }
                let userId = seatInfo.userInfo.userID
                if !userId.isEmpty {
                    if let volume = userVolumeMap[userId], volume > 25 {
                        seatView.isSpeaking = true
                        seatView.volume = volume
                    } else {
                        seatView.isSpeaking = false
                    }
                }
            }
            .store(in: &view.cancellableSet)

        seatInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak view] seatInfo in
                guard let seatView = view else { return }
                let userId = seatInfo.userInfo.userID
                if !userId.isEmpty {
                    seatView.isAudioMuted = (seatInfo.userInfo.microphoneStatus == .off)
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
    
    func subscribeState() {
        subscribeViewState()
        subscribeSeatState()
        setupliveEventListener()
        setupHostEventListener()
        setupGuestEventListener()
    }
    
    private func subscribeViewState() {
        subscribeViewState(StateSelector(projector: {  $0 }))
            .receive(on: RunLoop.main)
            .sink { [weak self] state in
                guard let self = self else { return }
                self.updateSeatGridLayout(layoutConfig: state.layoutConfig)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatState() {
        seatStore.state.subscribe(StatePublisherSelector(keyPath: \LiveSeatState.seatList))
            .map { $0.count }
            .filter { $0 != 0 }
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] seatCount in
                guard let self = self else { return }
                self.viewManager.onSeatCountChanged(seatCount: seatCount)
                self.seatContainerView.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    private func updateSeatGridLayout(layoutConfig: SGSeatViewLayoutConfig) {
        let layout = SeatGridViewLayout(rowSpacing: layoutConfig.rowSpacing, rowConfigs: layoutConfig.rowConfigs)
        seatContainerView.collectionViewLayout.invalidateLayout()
        seatContainerView.setCollectionViewLayout(layout, animated: true)
    }
    
    private func isAutoOnSeat() -> Bool {
        let selfId = TUIRoomEngine.getSelfInfo().userId
        let liveInfo = liveListStore.state.value.currentLive
        
        return selfId == liveInfo.liveOwner.userID ||
               liveInfo.seatMode == .free
    }
    
    private func notifyObserverEvent(notifyAction: @escaping (_ observer: SeatGridViewObserver) -> Void) {
        Task {
            await observers.notifyObservers(callback: notifyAction)
        }
    }
    
    private func subscribeViewState<Value>(_ selector: StateSelector<SGViewState, Value>) -> AnyPublisher<Value, Never> {
       return viewManager.observerState.subscribe(selector)
   }
    

    private func notifyEnterRoom() {
        DispatchQueue.main.async {
            NotificationCenter.default.post(Notification(name: SeatGridViewOnEnterRoomNotifyName))
        }
    }
    private func notifyExitRoom() {
        DispatchQueue.main.async {
            NotificationCenter.default.post(Notification(name: SeatGridViewOnExitRoomNotifyName))
        }
    }
    
}
