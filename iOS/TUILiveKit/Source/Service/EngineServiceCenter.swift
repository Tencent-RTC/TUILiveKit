//
//  AppEnviroment.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//
import RTCRoomEngine
import Combine

protocol BaseServiceProtocol {
    var roomEngine: TUIRoomEngine { get set }
    init(roomEngine: TUIRoomEngine)
}

// This object holds an instance of the RoomEngine service.
class ServiceCenter: NSObject {
    let mediaService: MediaService
    let roomService: RoomService
    let userService: UserService
    let seatService: SeatService
    let beautyService: BeautyService
    let errorService: ErrorService = ErrorService()
    
    let roomEngine: TUIRoomEngine
    
    weak var store: LiveStoreProvider?
    
    init(store: LiveStoreProvider) {
        self.store = store
        self.roomEngine = ServiceCenter.getRoomEngine()
        mediaService = MediaService(roomEngine: roomEngine)
        roomService = RoomService(roomEngine: roomEngine)
        userService = UserService(roomEngine: roomEngine)
        seatService = SeatService(roomEngine: roomEngine)
        beautyService = BeautyService(roomEngine: roomEngine)
        super.init()
        roomEngine.addObserver(self)
        
        guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else { return }
        liveListManager.addObserver(self)
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
        self.store = nil
        guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else { return }
        liveListManager.removeObserver(self)
    }
    
    static func getRoomEngine() -> TUIRoomEngine {
        LiveKitLog.info("\(#file)", "\(#line)","getRoomEngine")
        let jsonObject: [String: String] = ["api" : "createSubRoom"]
        guard let jsonData = try? JSONSerialization.data(withJSONObject: jsonObject, options: []) else {
            assert(false, "convert to jsonData error, jsonObject: \(jsonObject)")
            LiveKitLog.error("\(#file)", "\(#line)", "convert to jsonData error, jsonObject: \(jsonObject)")
            return TUIRoomEngine.sharedInstance()
        }
        guard let jsonString = String(data: jsonData, encoding: .utf8) else {
            assert(false, "convert to jsonString error, jsonObject: \(jsonObject)")
            LiveKitLog.error("\(#file)", "\(#line)", "convert to jsonString error, jsonObject: \(jsonObject)")
            return TUIRoomEngine.sharedInstance()
        }
        guard let roomEngine = TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString) as? TUIRoomEngine else {
            assert(false, "getRoomEngine error, jsonString: \(jsonString)")
            LiveKitLog.error("\(#file)", "\(#line)","getRoomEngine error, jsonString: \(jsonString)")
            return TUIRoomEngine.sharedInstance()
        }
        return roomEngine
    }
}

extension ServiceCenter: TUIRoomObserver {
    func onRoomNameChanged(roomId: String, roomName: String) {
        
    }
    
    func onRoomSeatModeChanged(roomId: String, seatMode: TUISeatMode) {
        guard let store = self.store else { return }
        if roomId == store.selectCurrent(RoomSelectors.getRoomId) {
            store.dispatch(action: RoomActions.updateRoomSeatModeByAdmin(payload: seatMode))
        }
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        guard let store = self.store else { return }
        let currentUserId = store.selectCurrent(UserSelectors.currentUserId)
        while true {
            let isSelfSeated = seatedList.contains { $0.userId != nil && $0.userId == currentUserId }
            if isSelfSeated {
                store.dispatch(action: MediaActions.operateMicrophoneMute(payload: false))
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
        store?.dispatch(action: UserActions.onUserVoiceVolumeChanged(payload: speakUsers))
    }
    
    // MARK: - room event.
    func onRoomDismissed(roomId: String) {
        let toast = ToastInfo(message: .roomDismissed)
        let onRoomDismissedActions: [any Action] = [
            MediaActions.cameraClosed(),
            MediaActions.microphoneClosed(),
            MediaActions.stopLocalPreview(),
            ViewActions.toastEvent(payload: toast),
            ViewActions.updateLiveStatus(payload: .finished),
            OperationActions.clearAllState(),
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
    
    func onKickedOffLine(message: String) {
        handlingKickedEvents(message: message)
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        if reason != .byServer {
            handlingKickedEvents(message: message)
        }
    }
}

extension ServiceCenter: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        guard let store = self.store else { return }
        if store.selectCurrent(UserSelectors.isOwner) { return }
        if modifyFlag == .category {
            if let categoryValue = liveInfo.categoryList.first?.intValue,
               let category = LiveStreamCategory(rawValue: categoryValue) {
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
            }
        }
        
        if modifyFlag == .coverUrl {
            store.dispatch(action: RoomActions.updateRoomCoverUrl(payload: liveInfo.coverUrl))
        }
        
        if modifyFlag == .publish {
            store.dispatch(action: RoomActions.updateRoomMode(payload: liveInfo.isPublicVisible ? .public : .privacy))
        }
    }
}

extension ServiceCenter {
    private func handlingKickedEvents(message: String) {
        guard let store = store else { return }
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: message)))
        if !store.selectCurrent(RoomSelectors.getRoomId).isEmpty {
            let kickedActions: [any Action] = [
                MediaActions.cameraClosed(),
                MediaActions.microphoneClosed(),
                MediaActions.stopLocalPreview(),
            ]
            kickedActions.forEach { action in
                store.dispatch(action: action)
            }
            store.dispatch(action: ViewActions.updateLiveStatus(payload: .none))
        }
    }
}

fileprivate extension String {
    static let roomDismissed = localized("live.audience.mask.title")
    static let kickedOutOfSeat = localized("live.seat.kickedOutOfSeat")
}
