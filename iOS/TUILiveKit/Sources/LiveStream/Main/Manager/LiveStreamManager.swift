//
//  LiveStreamManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

typealias LSRoomStateUpdateClosure = (inout LSRoomState) -> Void
typealias LSUserStateUpdateClosure = (inout LSUserState) -> Void
typealias LSMediaStateUpdateClosure = (inout LSMediaState) -> Void
typealias LSCoGuestStateUpdateClosure = (inout LSCoGuestState) -> Void

protocol LiveStreamManagerProvider: NSObject {
    func getCoreViewState<T: State>() -> T
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never>
}

class LiveStreamManager {
    
    public let toastSubject = PassthroughSubject<String, Never>()
    public let likeSubject = PassthroughSubject<Void, Never>()
    public let floatWindowSubject = PassthroughSubject<Void, Never>()
    
    class Context {
        let service = LSRoomEngineService()
        let coHostService = LSCoHostServiceImpl()
        weak var provider: LiveStreamManagerProvider?
        
        private(set) lazy var roomManager = LSRoomManager(context: self)
        private(set) lazy var userManager = LSUserManager(context: self)
        private(set) lazy var mediaManager = LSMediaManager(context: self)
        private(set) lazy var coGuestManager = LSCoGuestManager(context: self)
        
        private(set) lazy var coHostManager = LSCoHostManager(context: self)
        private(set) lazy var battleManager = LSBattleManager(context: self)
        
        private(set) lazy var engineObserver = LSRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = LSLiveListObserver(context: self)
        private(set) lazy var coHostObserver = LSCoHostObserver(context: self)
        
        let toastSubject: PassthroughSubject<String, Never>
        
        init(provider: LiveStreamManagerProvider, toastSubject: PassthroughSubject<String, Never>) {
            self.toastSubject = toastSubject
            self.provider = provider
            service.addEngineObserver(engineObserver)
            service.addLiveListManagerObserver(liveListObserver)
            coHostService.addConnectionObserver(coHostObserver)
        }
        
        deinit {
            service.removeEngineObserver(engineObserver)
            service.removeLiveListManagerObserver(liveListObserver)
            coHostService.removeConnectionObserver(coHostObserver)
        }
    }
    
    private let context: Context
    
    init(provider: LiveStreamManagerProvider) {
        self.context = Context(provider: provider, toastSubject: toastSubject)
    }
    
    func resetAllState() {
        context.roomManager.resetState()
        context.userManager.resetState()
        context.mediaManager.resetState()
        context.coGuestManager.resetState()
    }
    
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        guard let provider = context.provider else { return Empty<Value, Never>().eraseToAnyPublisher() }
        return provider.subscribeCoreViewState(selector)
    }
}

// MARK: - Room API
extension LiveStreamManager {
    var roomState: LSRoomState {
        context.roomManager.roomState
    }
    
    func subscribeRoomState<Value>(_ selector: StateSelector<LSRoomState, Value>) -> AnyPublisher<Value, Never> {
        context.roomManager.subscribeState(selector)
    }
    
    func getDefaultRoomName() -> String {
        return context.roomManager.getDefaultRoomName()
    }
    
    func syncLiveInfoToService() {
        context.roomManager.syncLiveInfoToService()
    }
    
    func updateRoomState(roomInfo: TUIRoomInfo) {
        context.roomManager.updateRoomState(roomInfo: roomInfo)
    }
    
    func update(roomState: LSRoomStateUpdateClosure) {
        context.roomManager.update(roomState: roomState)
        context.coHostManager.update(currentRoomId: context.roomManager.roomState.roomId)
    }
    
    func update(liveStatus: LiveStatus) {
        context.roomManager.update(liveStatus: liveStatus)
    }
    
    func update(roomCategory: LiveStreamCategory) {
        context.roomManager.update(roomCategory: roomCategory)
    }
    
    func update(roomPrivacy status: LiveStreamPrivacyStatus) {
        context.roomManager.update(roomPrivacy: status)
    }
    
    func update(roomCoverUrl: String) {
        context.roomManager.update(roomCoverUrl: roomCoverUrl)
    }
    
    func prepareLiveInfoBeforeEnterRoom(liveInfo: TUILiveInfo) {
        context.roomManager.updateLiveInfo(liveInfo: liveInfo)
    }
    
    func prepareRoomIdBeforeEnterRoom(roomId: String) {
        context.roomManager.update(roomId: roomId)
    }
    
    func fetchLiveInfo(roomId: String, onSuccess: @escaping ((_ liveInfo: TUILiveInfo)->()), onError: @escaping ((_ error: InternalError)->())) {
        Task {
            do {
                let liveInfo = try await context.roomManager.fetchLiveInfo(roomId: roomId)
                DispatchQueue.main.async {
                    onSuccess(liveInfo)
                }
            } catch let err as InternalError {
                DispatchQueue.main.async {
                    onError(err)
                }
            }
        }
    }
}

// MARK: - Media API
extension LiveStreamManager {
    var mediaState: LSMediaState {
        context.mediaManager.mediaState
    }
    
    func subscribeMediaState<Value>(_ selector: StateSelector<LSMediaState, Value>) -> AnyPublisher<Value, Never> {
        context.mediaManager.subscribeState(selector)
    }
    
    func setLocalVideoView(_ view: UIView) {
        context.mediaManager.setLocalVideoView(view: view)
    }
    
    func onCameraOpened() {
        context.mediaManager.onCameraOpened()
    }
    
    func updateVideoQuality(quality: TUIVideoQuality) {
        context.mediaManager.updateVideoQuality(quality: quality)
    }
}

// MARK: - User API
extension LiveStreamManager {
    var userState: LSUserState {
        context.userManager.userState
    }
    
    func subscribeUserState<Value>(_ selector: StateSelector<LSUserState, Value>) -> AnyPublisher<Value, Never> {
        context.userManager.subscribeState(selector)
    }
    
    func fetchAudienceList() {
        context.userManager.fetchAudienceList()
    }
    
    func muteAllRemoteAudio(isMute: Bool) {
        context.userManager.muteAllRemoteAudio(isMute: isMute)
    }
    
    func updateOwnerUserInfo() {
        context.userManager.updateOwnerUserInfo()
    }
    
    func updateSelfUserInfo() {
        context.userManager.updateSelfUserInfo()
    }
    
    func initSelfUserData() {
        context.userManager.initSelfUserData()
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await context.userManager.getUserInfo(userId: userId)
    }
}

// MARK: - CoGuest API
extension LiveStreamManager {
    var coGuestState: LSCoGuestState {
        context.coGuestManager.coGuestState
    }
    
    func subscribeCoGuestState<Value>(_ selector: StateSelector<LSCoGuestState, Value>) -> AnyPublisher<Value, Never> {
        context.coGuestManager.subscribeState(selector)
    }
    
    func fetchSeatList() {
        context.coGuestManager.fetchSeatList()
    }
    
    func fetchSeatApplicationList() {
        context.coGuestManager.fetchSeatApplicationList()
    }
    
    func removeSeatApplication(userId: String) {
        context.coGuestManager.removeSeatApplication(userId: userId)
    }
    
    func update(coGuestStatus: CoGuestStatus) {
        context.coGuestManager.update(coGuestStatus: coGuestStatus)
    }
    
    func onSeatListChanged(userList: [TUIUserInfo], joinList: [TUIUserInfo], leaveList: [TUIUserInfo]) {
        context.coGuestManager.onSeatListChanged(userList: userList, joinList: joinList, leaveList: leaveList)
    }
    
    func onRequestReceived(inviter: TUIUserInfo) {
        context.coGuestManager.onRequestReceived(inviter: inviter)
    }
    
    func onRequestCancelled(inviter: TUIUserInfo) {
        context.coGuestManager.onRequestCancelled(inviter: inviter)
    }
    
    func onUserConnectionAccepted(userId: String) {
        context.coGuestManager.onUserConnectionAccepted(userId: userId)
    }
    
    func onUserConnectionRejected(userId: String) {
        context.coGuestManager.onUserConnectionRejected(userId: userId)
    }
    
    func onUserConnectionTimeout(userId: String) {
        context.coGuestManager.onUserConnectionTimeout(userId: userId)
    }
    
    func onKickedOffSeat() {
        context.coGuestManager.onKickedOffSeat()
    }
}

// MARK: - CoHost API
extension LiveStreamManager {
    var coHostState: LSCoHostState {
        context.coHostManager.state
    }
    
    func subscribeCoHostState<Value>(_ selector: StateSelector<LSCoHostState, Value>) -> AnyPublisher<Value, Never> {
        context.coHostManager.subscribeCoHostState(selector)
    }
    
    var coHostManager: LSCoHostManager {
        context.coHostManager
    }
    
    var battleManager: LSBattleManager {
        context.battleManager
    }
}

// MARK: - Battle API
extension LiveStreamManager {
    var battleState: LSBattleState {
        context.battleManager.state
    }
    
    func subscribeBattleState<Value>(_ selector: StateSelector<LSBattleState, Value>) -> AnyPublisher<Value, Never> {
        context.battleManager.subscribeState(selector)
    }
}

extension LiveStreamManager: GiftListPanelDataSource {
    func getAnchorInfo() -> GiftUser {
        let owner = roomState.ownerInfo
        let giftUser = GiftUser(userId: owner.userId, name: owner.name, avatarUrl: owner.avatarUrl)
        return giftUser
    }
}

extension LiveStreamManager {
    var coreRoomState: RoomState {
        context.coreRoomState
    }
    var coreUserState: UserState {
        context.coreUserState
    }
    var coreMediaState: MediaState {
        context.coreMediaState
    }
    var coreCoHostState: CoHostState {
        context.coreCoHostState
    }
    var coreCoGuestState: CoGuestState {
        context.coreCoGuestState
    }
}

extension LiveStreamManager.Context {
    var coreRoomState: RoomState {
        guard let provider = provider else { return RoomState() }
        return provider.getCoreViewState()
    }
    var coreUserState: UserState {
        guard let provider = provider else { return UserState() }
        return provider.getCoreViewState()
    }
    var coreMediaState: MediaState {
        guard let provider = provider else { return MediaState() }
        return provider.getCoreViewState()
    }
    var coreCoHostState: CoHostState {
        guard let provider = provider else { return CoHostState() }
        return provider.getCoreViewState()
    }
    var coreCoGuestState: CoGuestState {
        guard let provider = provider else { return CoGuestState() }
        return provider.getCoreViewState()
    }
}
