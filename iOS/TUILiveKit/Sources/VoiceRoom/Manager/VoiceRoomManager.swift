//
//  VoiceRoomManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import RTCRoomEngine
import RTCCommon
import Combine
import TUILiveResources
import LiveStreamCore

protocol VoiceRoomManagerProvider: NSObject {
    func getCoreViewState<T: State>() -> T
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never>
}

class VoiceRoomManager {
    // Event for show toast
    public let toastSubject = PassthroughSubject<String, Never>()
    // Event for exit room
    public let exitSubject = PassthroughSubject<Void, Never>()
    // Event for click like button
    public let likeSubject = PassthroughSubject<Void, Never>()
    
    private let context: Context
    init(provider: VoiceRoomManagerProvider) {
        self.context = Context(provider: provider, toastSubject: toastSubject, exitSubject: exitSubject)
        provider.subscribeCoreViewState(StateSelector(keyPath: \SGSeatState.seatList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] seatList in
                guard let self = self else { return }
                userManager.onSeatListChanged(seatList: seatList)
            }
            .store(in: &context.cancellableSet)
    }
    
    class Context {
        let service = VRRoomEngineService()
        weak var provider: VoiceRoomManagerProvider?
        
        var cancellableSet = Set<AnyCancellable>()
        
        private(set) lazy var roomManager = VRRoomManager(context: self)
        private(set) lazy var userManager = VRUserManager(context: self)
        private(set) lazy var seatManager = VRSeatManager(context: self)
        
        private(set) lazy var engineObserver = VRRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = VRLiveListObserver(context: self)
        private(set) lazy var imObserver = VRIMObserver(context: self)
        
        let toastSubject: PassthroughSubject<String, Never>
        let exitSubject: PassthroughSubject<Void, Never>
        
        init(provider: VoiceRoomManagerProvider, toastSubject: PassthroughSubject<String, Never>, exitSubject: PassthroughSubject<Void, Never>) {
            self.toastSubject = toastSubject
            self.exitSubject = exitSubject
            self.provider = provider
            service.addEngineObserver(engineObserver)
            service.addLiveListObserver(liveListObserver)
            service.addIMFriendshipObserver(imObserver)
        }
        
        deinit {
            service.removeEngineObserver(engineObserver)
            service.removeLiveListObserver(liveListObserver)
        }
    }
    
    private func resetAllState() {
        roomManager.resetState()
        userManager.resetState()
        seatManager.resetState()
    }
}

// MARK: - Common
extension VoiceRoomManager {
    func onError(_ message: String) {
        toastSubject.send(message)
    }
    
    func prepareRoomIdBeforeEnterRoom(roomId: String, roomParams: RoomParams?) {
        roomManager.prepareRoomIdBeforeEnterRoom(roomId: roomId, roomParams: roomParams)
    }
    
    func onJoinVoiceRoom(roomInfo: TUIRoomInfo) {
        userManager.onJoinVoiceRoom(ownerId: roomInfo.ownerId)
        roomManager.onJoinVoiceRoom(roomInfo: roomInfo)
    }
    
    func onReceiveGift(price: Int, senderUserId: String) {
        roomManager.onReceiveGift(price: price, senderUserId: senderUserId)
    }
    
    func followUser(_ user: TUIUserInfo, isFollow: Bool) {
        userManager.followUser(user, isFollow: isFollow)
    }
}

// MARK: - Anchor
extension VoiceRoomManager {
    func onSetRoomName(_ name: String) {
        roomManager.onSetRoomName(name)
    }
    
    func onSetRoomPrivacy(_ mode: LiveStreamPrivacyStatus) {
        roomManager.onSetRoomPrivacy(mode)
    }
    
    func onSetRoomCoverUrl(_ coverUrl: String) {
        roomManager.onSetRoomCoverUrl(coverUrl)
    }
    
    func onSetRoomBackgroundUrl(_ backgroundUrl: String, isSetToService: Bool = false) {
        roomManager.onSetRoomBackgroundUrl(backgroundUrl, isSetToService: isSetToService)
    }
    
    func onChangedSeatMode(_ seatMode: TUISeatMode) {
        roomParams.seatMode = seatMode
    }
    
    func onStartVoiceRoom(roomInfo: TUIRoomInfo) {
        roomManager.onStartVoiceRoom(roomInfo: roomInfo)
        userManager.onStartVoiceRoom()
    }
    
    func onStopVoiceRoom() {
        userManager.onStopVoiceRoom()
    }
    
    func onApplyToTakeSeatRequestReceived(userInfo: TUIUserInfo) {
        seatManager.onApplyToTakeSeatRequestReceived(userInfo: userInfo)
    }
    
    func onApplyToTakeSeatRequestCancelled(userInfo: TUIUserInfo) {
        seatManager.onApplyToTakeSeatRequestCancelled(userInfo)
    }
    
    func onSentSeatInvitation(to userId: String) {
        seatManager.onSentSeatInvitation(to: userId)
    }
    
    func onRespondedSeatInvitation(of userId: String) {
        seatManager.onRespondedSeatInvitation(of: userId)
    }
}

// MARK: - Audience
extension VoiceRoomManager {
    func onLeaveVoiceRoom() {
        userManager.onLeaveVoiceRoom()
        resetAllState()
    }
    
    func onSentTakeSeatRequest() {
        seatManager.onSentTakeSeatRequest()
    }
    
    func onRespondedTakeSeatRequest() {
        seatManager.onRespondedTakeSeatRequest()
    }
    
    func onRespondedRemoteRequest() {
        seatManager.onRespondedRemoteRequest()
    }
    
    func onRemoteRequestError(userId: String) {
        seatManager.onRemoteRequestError(userId: userId)
    }
}

// MARK: - Subscribe
extension VoiceRoomManager {
    func subscribeState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        if let sel = selector as? StateSelector<VRUserState, Value> {
            return context.userManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<VRRoomState, Value> {
            return context.roomManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<VRSeatState, Value> {
            return context.seatManager.subscribeState(sel)
        }
        assert(false, "Not impl")
        return Empty<Value, Never>().eraseToAnyPublisher()
    }
    
    func subscribeCoreState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        guard let provider = context.provider else { return Empty<Value, Never>().eraseToAnyPublisher() }
        return provider.subscribeCoreViewState(selector)
    }
}
// MARK: - Tools
extension VoiceRoomManager {
    var roomState: VRRoomState {
        context.roomManager.state
    }
    var seatState: VRSeatState {
        context.seatManager.state
    }
    var userState: VRUserState {
        context.userManager.state
    }
    
    var roomParams: RoomParams {
        roomManager.roomParams
    }
    
    var roomManager: VRRoomManager {
        context.roomManager
    }
    var userManager: VRUserManager {
        context.userManager
    }
    var seatManager: VRSeatManager {
        context.seatManager
    }
}

extension VoiceRoomManager {
    var coreRoomState: SGRoomState {
        context.coreRoomState
    }
    var coreUserState: SGUserState {
        context.coreUserState
    }
    var coreMediaState: SGMediaState {
        context.coreMediaState
    }
    var coreSeatState: SGSeatState {
        context.coreSeatState
    }
}

extension VoiceRoomManager.Context {
    var coreRoomState: SGRoomState {
        guard let provider = provider else { return SGRoomState() }
        return provider.getCoreViewState()
    }
    var coreUserState: SGUserState {
        guard let provider = provider else { return SGUserState() }
        return provider.getCoreViewState()
    }
    var coreMediaState: SGMediaState {
        guard let provider = provider else { return SGMediaState() }
        return provider.getCoreViewState()
    }
    var coreSeatState: SGSeatState {
        guard let provider = provider else { return SGSeatState() }
        return provider.getCoreViewState()
    }
}

extension VoiceRoomManager: GiftListPanelProvider {
    func getAnchorInfo() -> GiftUser {
        let giftUser = GiftUser(userId: coreRoomState.ownerId, name: coreRoomState.ownerName, avatarUrl: coreRoomState.ownerAvatar)
        return giftUser
    }
}
