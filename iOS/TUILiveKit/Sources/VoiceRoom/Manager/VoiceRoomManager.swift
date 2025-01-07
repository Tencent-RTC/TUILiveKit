//
//  VoiceRoomManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import RTCRoomEngine
import RTCCommon
import Combine

// MARK: --------------- API --------------------
protocol VRRoomManagerInterface {
    func fetchRoomInfo()
    func fetchLiveInfo(roomId: String)
    func fetchRoomOwnerInfo(ownerId: String)
    
    func setRoomSeatModeByAdmin(_ seatMode: TUISeatMode)
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag)
    
    func update(roomId: String)
    func update(roomName: String)
    func update(ownerInfo: VRUser)
    func update(roomInfo: TUIRoomInfo)
    func update(giftIncome: Int, giftPeople: String)
    func update(roomParams: RoomParams)
    func update(roomCategory: LiveStreamCategory)
    func update(roomPrivacy: LiveStreamPrivacyStatus)
    func update(roomCoverUrl: String)
    func update(backgroundUrl: String)
    func update(seatMode: TUISeatMode)
}

protocol VRMediaManagerInterface {
    func update(microphoneMuted: Bool)
    func update(microphoneOpened: Bool)
}

protocol VRUserManagerInterface {
    func fetchUserList()
    func fetchSelfInfo()
    
    func followUser(_ user: VRUser, isFollow: Bool)
    func checkFollowType(_ userId: String)
    
    func update(linkStatus: LinkStatus)
}

protocol VRSeatManagerInterface {
    func fetchSeatList()
    func fetchSeatApplicationList()
    
    func addSeatUserInfo(_ info: TUIUserInfo)
    func removeSeatUserInfo(_ info: TUIUserInfo)
    
    func update(applicationStateIsApplying: Bool)
}

// MARK: --------------- IMPL --------------------
class VoiceRoomManager: VRRoomManagerInterface, VRMediaManagerInterface, VRUserManagerInterface, VRSeatManagerInterface {
    // TODO: Encapsulate the subscription logic of PassthroughSubject
    
    // Event for show toast
    public let toastSubject = PassthroughSubject<String, Never>()
    // Event for exit room
    public let exitSubject = PassthroughSubject<Void, Never>()
    // Event for click like button
    public let likeSubject = PassthroughSubject<Void, Never>()
    
    private let context: Context
    init() {
        self.context = Context(toastSubject: toastSubject, exitSubject: exitSubject)
    }
    
    class Context {
        let service = VRRoomEngineService()
        
        private(set) lazy var roomManager = VRRoomManager(context: self)
        private(set) lazy var userManager = VRUserManager(context: self)
        private(set) lazy var mediaManager = VRMediaManager(context: self)
        private(set) lazy var seatManager = VRSeatManager(context: self)
        
        private(set) lazy var engineObserver = VRRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = VRLiveListObserver(context: self)
        private(set) lazy var imObserver = VRIMObserver(context: self)
        
        let toastSubject: PassthroughSubject<String, Never>
        let exitSubject: PassthroughSubject<Void, Never>
        
        init(toastSubject: PassthroughSubject<String, Never>, exitSubject: PassthroughSubject<Void, Never>) {
            self.toastSubject = toastSubject
            self.exitSubject = exitSubject
            service.addEngineObserver(engineObserver)
            service.addLiveListObserver(liveListObserver)
            service.addIMFriendshipObserver(imObserver)
        }
        
        deinit {
            service.removeEngineObserver(engineObserver)
            service.removeLiveListObserver(liveListObserver)
        }
    }
    
    func resetAllState() {
        roomManager.resetState()
        userManager.resetState()
        mediaManager.resetState()
        seatManager.resetState()
    }
}

// MARK: - Subscribe
extension VoiceRoomManager {
    func subscribeRoomState<Value>(_ selector: StateSelector<VRRoomState, Value>) -> AnyPublisher<Value, Never> {
        context.roomManager.subscribeState(selector)
    }
    func subscribeRoomState() -> AnyPublisher<VRRoomState, Never> {
        context.roomManager.subscribeState()
    }
    
    func subscribeUserState<Value>(_ selector: StateSelector<VRUserState, Value>) -> AnyPublisher<Value, Never> {
        context.userManager.subscribeState(selector)
    }
    func subscribeUserState() -> AnyPublisher<VRUserState, Never> {
        context.userManager.subscribeState()
    }
    
    func subscribeMediaState<Value>(_ selector: StateSelector<VRMediaState, Value>) -> AnyPublisher<Value, Never> {
        context.mediaManager.subscribeState(selector)
    }
    func subscribeMediaState() -> AnyPublisher<VRMediaState, Never> {
        context.mediaManager.subscribeState()
    }
    
    func subscribeSeatState<Value>(_ selector: StateSelector<VRSeatState, Value>) -> AnyPublisher<Value, Never> {
        context.seatManager.subscribeState(selector)
    }
    func subscribeSeatState() -> AnyPublisher<VRSeatState, Never> {
        context.seatManager.subscribeState()
    }
}

// MARK: - Room
extension VoiceRoomManager {
    var roomManager: VRRoomManager {
        context.roomManager
    }
    var roomState: VRRoomState {
        roomManager.state
    }
    
    func fetchRoomInfo() {
        roomManager.fetchRoomInfo()
    }
    
    func fetchLiveInfo(roomId: String) {
        roomManager.fetchLiveInfo(roomId: roomId)
    }
    
    func fetchRoomOwnerInfo(ownerId: String) {
        roomManager.fetchRoomOwnerInfo(ownerId: ownerId)
    }
    
    func setRoomSeatModeByAdmin(_ seatMode: TUISeatMode) {
        roomManager.setRoomSeatModeByAdmin(seatMode)
    }
    
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        roomManager.setLiveInfo(liveInfo: liveInfo, modifyFlag: modifyFlag)
    }
    
    func update(roomId: String) {
        roomManager.update(roomId: roomId)
    }
    
    func update(roomName: String) {
        roomManager.update(roomName: roomName)
    }
    
    func update(ownerInfo: VRUser) {
        roomManager.update(ownerInfo: ownerInfo)
    }
    
    func update(roomInfo: TUIRoomInfo) {
        roomManager.update(roomInfo: roomInfo)
    }
    
    func update(giftIncome: Int, giftPeople: String) {
        roomManager.update(giftIncome: giftIncome, giftPeople: giftPeople)
    }
    
    func update(roomParams: RoomParams) {
        roomManager.update(roomParams: roomParams)
    }
    
    func update(roomCategory: LiveStreamCategory) {
        roomManager.update(roomCategory: roomCategory)
    }
    
    func update(roomPrivacy: LiveStreamPrivacyStatus) {
        roomManager.update(roomPrivacy: roomPrivacy)
    }
    
    func update(roomCoverUrl: String) {
        roomManager.update(roomCoverUrl: roomCoverUrl)
    }
    
    func update(backgroundUrl: String) {
        roomManager.update(backgroundUrl: backgroundUrl)
    }
    
    func update(seatMode: TUISeatMode) {
        roomManager.update(seatMode: seatMode)
    }
}

// MARK: - User
extension VoiceRoomManager {
    var userManager: VRUserManager {
        context.userManager
    }
    var userState: VRUserState {
        userManager.state
    }
    
    func fetchUserList() {
        userManager.fetchUserList()
    }
    
    func fetchSelfInfo() {
        userManager.fetchSelfInfo()
    }
    
    func followUser(_ user: VRUser, isFollow: Bool) {
        userManager.followUser(user, isFollow: isFollow)
    }
    
    func checkFollowType(_ userId:String) {
        userManager.checkFollowType(userId)
    }
    
    func update(linkStatus: LinkStatus) {
        userManager.update(linkStatus: linkStatus)
    }
}

// MARK: - Media
extension VoiceRoomManager {
    var mediaManager: VRMediaManager {
        context.mediaManager
    }
    var mediaState: VRMediaState {
        mediaManager.state
    }
    
    func update(microphoneMuted: Bool) {
        mediaManager.update(microphoneMuted: microphoneMuted)
    }
    
    func update(microphoneOpened: Bool) {
        mediaManager.update(microphoneOpened: microphoneOpened)
    }
}

// MARK: - Seat
extension VoiceRoomManager {
    var seatManager: VRSeatManager {
        context.seatManager
    }
    var seatState: VRSeatState {
        seatManager.state
    }
    
    func fetchSeatList() {
        seatManager.fetchSeatList()
    }
    
    func fetchSeatApplicationList() {
        seatManager.fetchSeatApplicationList()
    }
    
    func addSeatUserInfo(_ info: TUIUserInfo) {
        seatManager.addSeatUserInfo(info)
    }
    
    func removeSeatUserInfo(_ info: TUIUserInfo) {
        seatManager.removeSeatUserInfo(info)
    }
    
    func update(applicationStateIsApplying: Bool) {
        seatManager.update(applicationStateIsApplying: applicationStateIsApplying)
    }
}

extension VoiceRoomManager: GiftListPanelDataSource {
    func getAnchorInfo() -> GiftUser {
        let owner = roomState.ownerInfo
        let giftUser = GiftUser(userId: owner.userId, name: owner.name, avatarUrl: owner.avatarUrl)
        return giftUser
    }
}
