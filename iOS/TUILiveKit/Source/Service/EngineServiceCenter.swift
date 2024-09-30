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
    let connectionService: ConnectionService
    let battleService: BattleService
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
        connectionService = ConnectionService(roomEngine: roomEngine)
        battleService = BattleService(roomEngine: roomEngine)
        super.init()
        roomEngine.addObserver(self)
        
        if let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager {
            liveListManager.addObserver(self)
        }
        roomEngine.getLiveConnectionManager().addObserver(self)
        roomEngine.getLiveBattleManager().addObserver(self)
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
        self.store = nil
        guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else { return }
        liveListManager.removeObserver(self)
        
        roomEngine.getLiveConnectionManager().removeObserver(self)
        roomEngine.getLiveBattleManager().removeObserver(self)
    }
    
    static func getRoomEngine() -> TUIRoomEngine {
        LiveKitLog.info("\(#file)", "\(#line)","getRoomEngine")
        let jsonObject: [String: String] = ["api" : "createSubRoom"]
        guard let jsonData = try? JSONSerialization.data(withJSONObject: jsonObject, options: []) else {
            LiveKitLog.error("\(#file)", "\(#line)", "convert to jsonData error, jsonObject: \(jsonObject)")
            return TUIRoomEngine.sharedInstance()
        }
        guard let jsonString = String(data: jsonData, encoding: .utf8) else {
            LiveKitLog.error("\(#file)", "\(#line)", "getRoomEngine:[onError:[jsonObject: \(jsonObject)]")
            return TUIRoomEngine.sharedInstance()
        }
        guard let roomEngine = TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString) as? TUIRoomEngine else {
            LiveKitLog.error("\(#file)", "\(#line)","getRoomEngine:[onError:[jsonString: \(jsonString)]")
            return TUIRoomEngine.sharedInstance()
        }
        return roomEngine
    }
}

extension ServiceCenter: TUIRoomObserver {
    func onRoomNameChanged(roomId: String, roomName: String) {
        
    }
    
    func onRoomSeatModeChanged(roomId: String, seatMode: TUISeatMode) {
        LiveKitLog.info("\(#file)","\(#line)","onRoomSeatModeChanged:[roomId:\(roomId),seatMode:\(seatMode.rawValue)]")
        guard let store = self.store else { return }
        if roomId == store.selectCurrent(RoomSelectors.getRoomId) {
            store.dispatch(action: RoomActions.updateRoomSeatModeByAdmin(payload: seatMode))
        }
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        LiveKitLog.info("\(#file)","\(#line)",
                        "onSeatListChanged:[seatList:\(seatList),seatedList:\(seatedList),leftList:\(leftList)]")
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
        LiveKitLog.info("\(#file)","\(#line)","onKickedOffSeat:[userId:\(userId)]")
        guard let store = self.store else { return }
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .kickedOutOfSeat)))
    }
    
    func onRequestReceived(request: TUIRequest) {
        LiveKitLog.info("\(#file)","\(#line)","onRequestReceived:[request:\(request)]")
        guard let store = self.store else { return }
        switch request.requestAction {
        case .takeSeat:
            let seatApplication = SeatApplication(request: request)
            store.dispatch(action: SeatActions.addSeatApplication(payload: seatApplication))
            let actions: [ActionTemplate<User>] = [SeatActions.addSeatApplicationUser]
            let param = generateActionTemplateParamTuple(param: request.userId, actions: actions)
            store.dispatch(action: UserActions.fetchUserInfo(payload: param))
        case .remoteUserOnSeat:
            store.dispatch(action: SeatActions.updateReceivedSeatInvitation(payload: SeatInvitation(request: request)))
        default:
            break
        }
    }
    
    func onRequestCancelled(requestId: String, userId: String) {
        LiveKitLog.info("\(#file)","\(#line)","onRequestCancelled:[requestId:\(requestId),userId:\(userId)]")
        guard let store = self.store else { return }
        let isContainApplicationRequest = store.selectCurrent(SeatSelectors.getSeatApplications).contains { $0.id == requestId }
        if isContainApplicationRequest {
            store.dispatch(action: SeatActions.removeSeatApplication(payload: requestId))
        }
        
        if store.selectCurrent(SeatSelectors.getReceivedSeatInvitation).id == requestId {
            store.dispatch(action: SeatActions.updateReceivedSeatInvitation(payload: SeatInvitation()))
            store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .inviteCancelText)))
        }
    }
    
    func onRequestProcessed(requestId: String, userId: String) {
        LiveKitLog.info("\(#file)","\(#line)","onRequestProcessed:[requestId:\(requestId),userId:\(userId)]")
        guard let store = self.store else { return }
        let isContainRequest = store.selectCurrent(SeatSelectors.getSeatApplications).contains { $0.id == requestId }
        if isContainRequest {
            store.dispatch(action: SeatActions.removeSeatApplication(payload: requestId))
        }
    }
    
    // MARK: - Media event.
    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        LiveKitLog.info("\(#file)","\(#line)",
                        "onUserVideoStateChanged:[userId:\(userId),streamType:\(streamType.rawValue),hasVideo:\(hasVideo),reason:\(reason.rawValue)]")
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
        LiveKitLog.info("\(#file)","\(#line)","onUserAudioStateChanged:[userId:\(userId),hasAudio:\(hasAudio),reason:\(reason.rawValue)]")
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
        LiveKitLog.info("\(#file)","\(#line)","onRoomDismissed:[roomId:\(roomId)]")
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
        LiveKitLog.info("\(#file)","\(#line)","onRemoteUserEnterRoom:[roomId:\(roomId),userId:\(userInfo.userId)]")
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.roomOwnerId) == userInfo.userId { return }
        store.dispatch(action: UserActions.onUserEnterRoom(payload: User(userInfo: userInfo)))
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveKitLog.info("\(#file)","\(#line)","onRemoteUserLeaveRoom:[roomId:\(roomId),userId:\(userInfo.userId)]")
        guard let store = self.store else { return }
        if store.selectCurrent(RoomSelectors.roomOwnerId) == userInfo.userId { return }
        store.dispatch(action: UserActions.onUserLeaveRoom(payload: User(userInfo: userInfo)))
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        LiveKitLog.info("\(#file)","\(#line)","onRoomUserCountChanged:[roomId:\(roomId),userCount:\(userCount)]")
        let audienceCount = userCount > 0 ? userCount - 1 : userCount
        store?.dispatch(action: RoomActions.updateRoomMemberCount(payload: audienceCount))
    }
    
    func onKickedOffLine(message: String) {
        LiveKitLog.info("\(#file)","\(#line)","onKickedOffLine:[message:\(message)]")
        handlingKickedEvents(message: message)
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        LiveKitLog.info("\(#file)","\(#line)","onKickedOutOfRoom:[roomId:\(roomId),reason:\(reason.rawValue),message:\(message)]")
        if reason != .byServer {
            handlingKickedEvents(message: message)
        }
    }
}

extension ServiceCenter: TUILiveListManagerObserver {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        guard let store = self.store else { return }
        if store.selectCurrent(UserSelectors.isOwner) { return }
        if modifyFlag.contains(.category) {
            if let categoryValue = liveInfo.categoryList.first?.intValue,
               let category = LiveStreamCategory(rawValue: categoryValue) {
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
            }
        }
        
        if modifyFlag.contains(.backgroundUrl) {
            store.dispatch(action: RoomActions.updateRoomBackgroundUrl(payload: liveInfo.backgroundUrl))
        }
        
        if modifyFlag.contains(.coverUrl) {
            store.dispatch(action: RoomActions.updateRoomCoverUrl(payload: liveInfo.coverUrl))
        }
        
        if modifyFlag.contains(.publish) {
            store.dispatch(action: RoomActions.updateRoomMode(payload: liveInfo.isPublicVisible ? .public : .privacy))
        }
    }
}

extension ServiceCenter: TUILiveConnectionObserver {
    
    func onConnectionUserListChanged(connectedList: [TUIConnectionUser], joinedList: [TUIConnectionUser], leavedList: [TUIConnectionUser]) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionUserListChanged:[connectedList:\(connectedList),joinedList:\(joinedList),leavedList:\(leavedList)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionUserListChanged(payload: connectedList))
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser, inviteeList: [TUIConnectionUser], extensionInfo: String) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionRequestReceived:[inviter:\(inviter),inviteeList:\(inviteeList),extensionInfo:\(extensionInfo)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionRequestReceived(payload: (inviter, inviteeList, extensionInfo)))
    }
    
    func onConnectionRequestCancelled(inviter: TUIConnectionUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionRequestCancelled:[inviter:\(inviter)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionRequestCancelled(payload: inviter))
    }
    
    func onConnectionRequestAccept(invitee: TUIConnectionUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionRequestAccept:[invitee:\(invitee)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionRequestAccept(payload: invitee))
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionRequestReject:[invitee:\(invitee)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionRequestReject(payload: invitee))
    }
    
    func onConnectionRequestTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onConnectionRequestTimeout:[inviter:\(inviter),invitee:\(invitee)]")
        guard let store = self.store else { return }
        store.dispatch(action: ConnectionActions.onConnectionRequestTimeout(payload: (store.roomState.roomId, inviter, invitee)))
    }
}

extension ServiceCenter: TUILiveBattleObserver {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        LiveKitLog.info("\(#file)", "\(#line)", "onBattleStarted:[battleInfo:\(battleInfo)]")
        handleBattleStarted(battleInfo: battleInfo)
    }
    
    private func handleBattleStarted(battleInfo: TUIBattleInfo) {
        guard let store = self.store else { return }
        battleInfo.config.duration = battleInfo.config.duration + Double(battleInfo.startTime) - Date().timeIntervalSince1970
       
        store.dispatch(action: BattleActions.onBattleStarted(payload: battleInfo))
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo, reason: TUIBattleStoppedReason) {
        LiveKitLog.info("\(#file)", "\(#line)","onBattleEnded:[battleInfo:\(battleInfo),reason:\(reason)]")
        guard let store = store else { return }
        store.dispatch(action: BattleActions.onBattleEnded(payload:(battleInfo, reason)))
    }
    
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser) {
        LiveKitLog.info("\(#file)", "\(#line)","onUserJoinBattle:[battleId:\(battleId),battleUser:\(battleUser)]")
        store?.dispatch(action: BattleActions.onUserJoinBattle(payload:(battleUser)))
    }
    
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser) {
        LiveKitLog.info("\(#file)", "\(#line)","onUserExitBattle:[battleId:\(battleId),battleUser:\(battleUser)]")
        store?.dispatch(action: BattleActions.onUserExitBattle(payload:(battleUser)))
    }
    
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser]) {
        LiveKitLog.info("\(#file)", "\(#line)","onBattleScoreChanged:[battleId:\(battleId),battleUserList:\(battleUserList)]")
        store?.dispatch(action: BattleActions.onBattleScoreChanged(payload:(battleUserList)))
    }
    
    func onBattleRequestReceived(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        LiveKitLog.info("\(#file)", 
                        "\(#line)",
                        "onBattleRequestReceived:[battleInfo:\(battleInfo),inviter:\(inviter),invitee:\(invitee)]")
        guard let store = self.store else { return}
        store.dispatch(action: BattleActions.onBattleRequestReceived(payload:(battleInfo.battleId, inviter)))
    }
    
    func onBattleRequestCancelled(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onBattleRequestCancelled:[battleInfo:\(battleInfo),inviter:\(inviter),invitee:\(invitee)]")
        guard let store = store else { return }
        store.dispatch(action: BattleActions.onBattleRequestCancelled(payload:(inviter)))
        
        let message = String.localizedReplace(.battleInvitationCancelledText, replace: inviter.userName)
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: message, position: .center)))
    }
    
    func onBattleRequestTimeout(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onBattleRequestTimeout:[battleInfo:\(battleInfo),inviter:\(inviter),invitee:\(invitee)]")
        guard let store = store else { return }
        store.dispatch(action: BattleActions.onBattleRequestTimeout(payload:(inviter, invitee)))
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .battleInvitationTimeoutText, position: .center)))
    }
    
    func onBattleRequestAccept(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onBattleRequestAccept:[battleInfo:\(battleInfo),inviter:\(inviter),invitee:\(invitee)]")
        guard let store = store else { return }
        store.dispatch(action: BattleActions.onBattleRequestAccept(payload:(invitee)))
    }
    
    func onBattleRequestReject(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        LiveKitLog.info("\(#file)",
                        "\(#line)",
                        "onBattleRequestReject:[battleInfo:\(battleInfo),inviter:\(inviter),invitee:\(invitee)]")
        guard let store = store else { return }
        store.dispatch(action: BattleActions.onBattleRequestReject(payload:(invitee)))
        
        let message = String.localizedReplace(.battleInvitationRejectText, replace: invitee.userName)
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: message, position: .center)))
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
    static let inviteCancelText = localized("live.seat.inviteSeatCancel")
    static let battleInvitationTimeoutText = localized("live.battle.invitation.timeout")
    static let battleInvitationRejectText = localized("live.battle.invitation.reject.xxx")
    static let battleInvitationCancelledText = localized("live.battle.invitation.cancelled.xxx")
}
