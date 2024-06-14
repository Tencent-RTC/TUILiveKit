//
//  AppEnviroment.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//
import RTCRoomEngine
import Combine


// This object holds an instance of the RoomEngine service.
class ServiceCenter: NSObject {
    let mediaService: MediaService = MediaService()
    let roomService: RoomService = RoomService()
    let userService: UserService = UserService()
    let seatService: SeatService = SeatService()
    let errorService: ErrorService = ErrorService()
    let beautyService: BeautyService = BeautyService()
    
    @WeakLazyInjected var store: LiveStore?
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    private let volumeSubject = CurrentValueSubject<Set<String>, Never>([])
    private var cancellableSet: Set<AnyCancellable> = []
    override init() {
        super.init()
        TUIRoomEngine.sharedInstance().addObserver(self)
        subscribe()
    }
    
    func subscribe() {
        let timeInterval :DispatchQueue.SchedulerTimeType.Stride = .milliseconds(500)
        volumeSubject
            .removeDuplicates()
            .debounce(for: timeInterval, scheduler: DispatchQueue.main)
            .sink { [weak self] speakUsers in
                guard let store = self?.store else { return }
                store.dispatch(action: UserActions.onUserVoiceVolumeChanged(payload: speakUsers))
            }
            .store(in: &cancellableSet)
    }
    
}


extension ServiceCenter: TUIRoomObserver {
    func onRoomNameChanged(roomId: String, roomName: String) {
        
    }
    
    func onRoomSeatModeChanged(roomId: String, seatMode: TUISeatMode) {
        
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        guard let store = self.store else { return }
        let currentUserId = store.selectCurrent(UserSelectors.currentUserId)
        while true {
            let isSelfSeated = seatedList.contains { $0.userId != nil && $0.userId == currentUserId }
            if isSelfSeated {
                store.dispatch(action: MediaActions.operateMicrophone(payload: true))
                if store.selectCurrent(ViewSelectors.autoOpenCameraOnSeated) {
                    store.dispatch(action: MediaActions.operateCamera(payload: true))
                }
                store.dispatch(action: ViewActions.updateLinkStatus(payload: .linking))
                break
            }
            let isSelfLeft = leftList.contains { $0.userId != nil && $0.userId == currentUserId }
            if isSelfLeft {
                store.dispatch(action: ViewActions.updateLinkStatus(payload: .none))
                store.dispatch(action: MediaActions.cameraClosed())
                store.dispatch(action: MediaActions.microphoneClosed())
            }
            break
        }
        let changedList = seatList.map { seatInfo in
            return SeatInfo(info: seatInfo)
        }
        store.dispatch(action: SeatActions.seatListChanged(payload: changedList))
    }
    
    func onKickedOffSeat(userId: String) {
        guard let store = self.store else { return }
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .kickedOutOfSeat)))
    }
    
    func onRequestReceived(request: TUIRequest) {
        guard let store = self.store else { return }
        switch request.requestAction {
            case .takeSeat:
                let seatApplication = SeatApplication(request: request)
                store.dispatch(action: SeatActions.addSeatApplication(payload: seatApplication))
                let actions: [ActionTemplate<User>] = [SeatActions.addSeatApplicationUser]
                let param = generateActionTemplateParamTuple(param: request.userId, actions: actions)
                store.dispatch(action: UserActions.fetchUserInfo(payload: param))
            default:
                break
        }
    }
    
    func onRequestCancelled(requestId: String, userId: String) {
        guard let store = self.store else { return }
        let isContainRequest = store.selectCurrent(SeatSelectors.getSeatApplications).contains { $0.id == requestId }
        if isContainRequest {
            store.dispatch(action: SeatActions.removeSeatApplication(payload: requestId))
        }
    }
    
    func onRequestProcessed(requestId: String, userId: String) {
        guard let store = self.store else { return }
        let isContainRequest = store.selectCurrent(SeatSelectors.getSeatApplications).contains { $0.id == requestId }
        if isContainRequest {
            store.dispatch(action: SeatActions.removeSeatApplication(payload: requestId))
        }
    }
    
    // MARK: - Media event.
    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        guard let store = self.store else { return }
        // TODO: - abyyxwang video status changed
        store.dispatch(action: UserActions.onUserVideoAvailable(payload: (userId, hasVideo)))
        switch reason {
            case .byAdmin:
                // TODO: - Toast handle
                break
            default:
                break
        }
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        guard let store = self.store else { return }
        store.dispatch(action: UserActions.onUserAudioAvailable(payload: (userId, hasAudio)))
        if userId == store.selectCurrent(UserSelectors.currentUserId) {
            let action = hasAudio ? MediaActions.microphoneUnmuted() : MediaActions.microphoneMuted()
            store.dispatch(action: action)
        }
        switch reason {
            case .byAdmin:
                // TODO: - Toast handle
                break
            default:
                break
        }
    }
    
    func onUserVoiceVolumeChanged(volumeMap: [String : NSNumber]) {
        let result = volumeMap
            .mapValues { $0.intValue }
            .flatMap { key, value in
                return value > 25 ? [key] : []
            }
        let speakUsers = Set(result)
        volumeSubject.send(speakUsers)
    }
    
    // MARK: - room event.
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
    }
    
    func onRoomDismissed(roomId: String) {
        let toast = ToastInfo(message: .roomDismissed)
        let onRoomDismissedActions: [any Action] = [
            MediaActions.cameraClosed(),
            MediaActions.microphoneClosed(),
            MediaActions.stopLocalPreview(),
            ViewActions.toastEvent(payload: toast),
            ViewActions.updateLiveStatus(payload: .finished),
        ]
        onRoomDismissedActions.forEach { action in
            store?.dispatch(action: action)
        }
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.roomOwnerId) == userInfo.userId { return }
        store.dispatch(action: UserActions.onUserEnterRoom(payload: User(userInfo: userInfo)))
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.roomOwnerId) == userInfo.userId { return }
        store.dispatch(action: UserActions.onUserLeaveRoom(payload: User(userInfo: userInfo)))
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        let audienceCount = userCount > 0 ? userCount - 1 : userCount
        store?.dispatch(action: RoomActions.updateRoomMemberCount(payload: audienceCount))
    }
}

fileprivate extension String {
    static let roomDismissed = localized("live.audience.mask.title")
    static let kickedOutOfSeat = localized("live.seat.kickedOutOfSeat")
}
