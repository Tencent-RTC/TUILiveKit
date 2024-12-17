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

typealias LSRoomStateUpdateClosure = (inout LSRoomState) -> Void
typealias LSUserStateUpdateClosure = (inout LSUserState) -> Void
typealias LSMediaStateUpdateClosure = (inout LSMediaState) -> Void
typealias LSCoGuestStateUpdateClosure = (inout LSCoGuestState) -> Void

class LiveStreamManager {
    
    public let toastSubject = PassthroughSubject<String, Never>()
    public let likeSubject = PassthroughSubject<Void, Never>()
    
    class Context {
        let service = LSRoomEngineService()
        let coHostService = LSCoHostServiceImpl()
        let battleService = EngineBattleService()
        
        private(set) lazy var roomManager = LSRoomManager(context: self)
        private(set) lazy var userManager = LSUserManager(context: self)
        private(set) lazy var mediaManager = LSMediaManager(context: self)
        private(set) lazy var coGuestManager = LSCoGuestManager(context: self)
        
        private(set) lazy var coHostManager = LSCoHostManager(context: self)
        private(set) lazy var battleManager = LSBattleManager(context: self)
        
        private(set) lazy var engineObserver = LSRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = LSLiveListObserver(context: self)
        private(set) lazy var coHostObserver = LSCoHostObserver(context: self)
        private(set) lazy var battleObserver = LSBattleManagerObserver(context: self)
        
        let toastSubject: PassthroughSubject<String, Never>
        
        init(toastSubject: PassthroughSubject<String, Never>) {
            self.toastSubject = toastSubject
            service.addEngineObserver(engineObserver)
            service.addLiveListManagerObserver(liveListObserver)
            service.addBattleObserver(battleObserver)
            coHostService.addConnectionObserver(coHostObserver)
        }
        
        deinit {
            service.removeEngineObserver(engineObserver)
            service.removeLiveListManagerObserver(liveListObserver)
            service.removeBattleObserver(battleObserver)
            coHostService.removeConnectionObserver(coHostObserver)
        }
    }
    
    private let context: Context
    
    init() {
        self.context = Context(toastSubject: toastSubject)
    }
    
    func resetAllState() {
        context.roomManager.resetState()
        context.userManager.resetState()
        context.mediaManager.resetState()
        context.coGuestManager.resetState()
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
}

// MARK: - Media API
extension LiveStreamManager {
    var mediaState: LSMediaState {
        context.mediaManager.mediaState
    }
    
    func subscribeMediaState<Value>(_ selector: StateSelector<LSMediaState, Value>) -> AnyPublisher<Value, Never> {
        context.mediaManager.subscribeState(selector)
    }
    
    func switchCamera() {
        context.mediaManager.switchCamera()
    }
    
    func setCameraMirror() {
        context.mediaManager.setCameraMirror()
    }
    
    func setLocalVideoView(_ view: UIView) {
        context.mediaManager.setLocalVideoView(view: view)
    }
    
    func openLocalCamera() {
        context.mediaManager.openLocalCamera()
    }
    
    func closeLocalCamera() {
        context.mediaManager.closeLocalCamera()
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
