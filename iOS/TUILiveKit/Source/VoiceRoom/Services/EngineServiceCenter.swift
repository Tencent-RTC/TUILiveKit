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
    
    @WeakLazyInjected var store: VoiceRoomStoreProvider?
    
    private let volumeSubject = CurrentValueSubject<Set<String>, Never>([])
    private var cancellables: Set<AnyCancellable> = []
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
            .store(in: &cancellables)
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
        seatedList.filter{ $0.userId != nil && $0.userId == currentUserId }.forEach { seatInfo in
            store.dispatch(action: MediaActions.operateMicrophone(payload: true))
        }
        let changedList = seatList.map { seatInfo in
            return SeatInfo(info: seatInfo)
        }
        store.dispatch(action: SeatActions.seatListChanged(payload: changedList))
    }
    
    func onKickedOffSeat(userId: String) {
        guard let store = self.store else { return }
        store.dispatch(action: ViewActions.toast(payload: ToastInfo(message: .kickedOutOfSeat)))
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
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        guard let store = self.store else { return }
        store.dispatch(action: UserActions.onUserAudioAvailable(payload: (userId, hasAudio)))
        if userId == store.selectCurrent(UserSelectors.currentUserId) {
            let action = hasAudio ? MediaActions.localAudioUnmuted() : MediaActions.localAudioMuted()
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
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
    }
    
    func onRoomDismissed(roomId: String) {
        let toast = ToastInfo(message: .roomDismissed)
        store?.dispatch(action: ViewActions.toast(payload: toast))
        store?.dispatch(action: ViewActions.endView())
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.getRoomOwnerId) == userInfo.userId { return }
        store.dispatch(action: UserActions.onUserEnterRoom(payload: User(userInfo: userInfo)))
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.getRoomOwnerId) == userInfo.userId { return }
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
