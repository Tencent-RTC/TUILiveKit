//
//  VoiceRoomManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import RTCRoomEngine
import RTCCommon
import Combine
import AtomicXCore

class VoiceRoomManager {
    // Event for show toast
    public let toastSubject = PassthroughSubject<String, Never>()
    // Event for exit room
    public let exitSubject = PassthroughSubject<Void, Never>()
    
    private let liveId: String
    private let context: Context
    init( liveId: String) {
        self.liveId = liveId
        self.context = Context(toastSubject: toastSubject, exitSubject: exitSubject)
        
        subscribeCoreState(StatePublisherSelector(keyPath: \LiveSeatState.seatList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] seatList in
                guard let self = self else { return }
                let tuiSeatLinfoList = seatList.map { TUISeatInfo(from: $0) }
                userManager.onSeatListChanged(seatList: tuiSeatLinfoList)
            }
            .store(in: &context.cancellableSet)
    }
    
    class Context {
        let service = VRRoomEngineService()
        
        var cancellableSet = Set<AnyCancellable>()
        
        private(set) lazy var roomManager = VRRoomManager(context: self)
        private(set) lazy var userManager = VRUserManager(context: self)
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
    
    func refreshSelfInfo() {
        userManager.refreshSelfInfo()
    }
    
    func onJoinVoiceRoom(liveInfo: TUILiveInfo) {
        userManager.onJoinVoiceRoom(ownerId: liveInfo.ownerId)
        roomManager.onJoinVoiceRoom(liveInfo: liveInfo)
    }
    
    func fetchGiftCount(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.roomManager.fetchGiftCount(roomId: roomId)
                onSuccess()
            } catch let error as InternalError {
                onError(error)
            }
        }
    }
    
    func fetchLikeCount(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.roomManager.fetchLikeCount(roomId: roomId)
                onSuccess()
            } catch let error as InternalError {
                onError(error)
            }
        }
    }
    
    func fetchViewCount(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.roomManager.fetchViewCount(roomId: roomId)
                onSuccess()
            } catch let error as InternalError {
                onError(error)
            }
        }
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

    func onSetlayoutType(layoutType: VoiceRoomLayoutType) {
        roomManager.onSetlayoutType(layoutType: layoutType)
    }

    func onSetRoomBackgroundUrl(_ backgroundUrl: String, isSetToService: Bool = false) {
        roomManager.onSetRoomBackgroundUrl(backgroundUrl, isSetToService: isSetToService)
    }
    
    func onChangedSeatMode(_ seatMode: TUISeatMode) {
        roomParams.seatMode = seatMode
    }
    
    func onStartVoiceRoom(liveInfo: TUILiveInfo) {
        roomManager.onStartVoiceRoom(liveInfo: liveInfo)
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

    func onSetHasKTVAbility(hasKTVAbility: Bool) {
        roomManager.onSetHasKTVAbility(hasKTVAbility: hasKTVAbility)
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
    
    func subscribeCoreState<State, Value>(_ selector: StatePublisherSelector<State, Value>) -> AnyPublisher<Value, Never> {
        if let sel = selector as? StatePublisherSelector<LiveListState, Value> {
            return liveListStore.state.subscribe(sel)
        } else if let sel = selector as? StatePublisherSelector<LiveSeatState, Value> {
            return liveSeatStore.state.subscribe(sel)
        }
        assert(false, "Not impl")
        return Empty<Value, Never>().eraseToAnyPublisher()
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
    var liveListStore: LiveListStore {
        return LiveListStore.shared
    }
    
    var liveSeatStore: LiveSeatStore {
        return LiveSeatStore.create(liveID: liveId)
    }
    
    var coreLiveState: AtomicLiveInfo {
        liveListStore.state.value.currentLive
    }
    
    var coreSeatState: LiveSeatState {
        liveSeatStore.state.value
    }
}
