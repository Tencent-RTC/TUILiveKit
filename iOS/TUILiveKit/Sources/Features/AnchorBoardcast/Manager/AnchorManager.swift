//
//  AnchorManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

typealias AnchorRoomStateUpdateClosure = (inout AnchorRoomState) -> Void
typealias AnchorUserStateUpdateClosure = (inout AnchorUserState) -> Void
typealias AnchorMediaStateUpdateClosure = (inout AnchorMediaState) -> Void
typealias AnchorCoGuestStateUpdateClosure = (inout AnchorCoGuestState) -> Void

typealias InternalErrorBlock = (_ error: InternalError) -> Void

typealias StateSelector = RTCCommon.StateSelector
typealias CoHostState = LiveStreamCore.CoHostState
typealias CoGuestState = LiveStreamCore.CoGuestState

protocol AnchorManagerProvider: NSObject {
    func getCoreViewState<T: State>() -> T
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never>
}

class AnchorManager {
    
    public let toastSubject = PassthroughSubject<String, Never>()
    public let floatWindowSubject = PassthroughSubject<Void, Never>()
    public let kickedOutSubject = PassthroughSubject<Bool, Never>() // bool value for room is dismissed
    public let onEndLivingSubject = PassthroughSubject<AnchorState, Never>()
    
    class Context {
        let service = AnchorService()
        let coHostService = AnchorCoHostServiceImpl()
        weak var provider: AnchorManagerProvider?
        
        private(set) lazy var roomManager = AnchorRoomManager(context: self)
        private(set) lazy var userManager = AnchorUserManager(context: self)
        private(set) lazy var mediaManager = AnchorMediaManager(context: self)
        private(set) lazy var coGuestManager = AnchorCoGuestManager(context: self)
        
        private(set) lazy var coHostManager = AnchorCoHostManager(context: self)
        private(set) lazy var battleManager = AnchorBattleManager(context: self)
        
        private(set) lazy var engineObserver = AnchorRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = AnchorLiveListObserver(context: self)
        private(set) lazy var coHostObserver = AnchorCoHostObserver(context: self)
        
        let toastSubject: PassthroughSubject<String, Never>
        let kickedOutSubject: PassthroughSubject<Bool, Never>
        
        init(provider: AnchorManagerProvider, toastSubject: PassthroughSubject<String, Never>, kickedOutSubject: PassthroughSubject<Bool, Never>) {
            self.toastSubject = toastSubject
            self.kickedOutSubject = kickedOutSubject
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
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(provider: AnchorManagerProvider) {
        self.context = Context(provider: provider, toastSubject: toastSubject, kickedOutSubject: kickedOutSubject)
    }
}

// MARK: - Common
extension AnchorManager {
    func onError(_ error: InternalError) {
        toastSubject.send(error.localizedMessage)
    }
    
    func onCameraOpened(localVideoView: UIView) {
        context.mediaManager.onCameraOpened()
        context.mediaManager.setLocalVideoView(view: localVideoView)
    }
}

// MARK: - Anchor
extension AnchorManager {
    func prepareLiveInfoBeforeEnterRoom(liveInfo: LiveInfo) {
        context.roomManager.prepareLiveInfoBeforeEnterRoom(liveInfo: liveInfo)
    }
    
    func onSetRoomName(_ name: String) {
        context.roomManager.onSetRoomName(name)
    }
    
    func onSetRoomPrivacy(_ mode: LiveStreamPrivacyStatus) {
        context.roomManager.onSetRoomPrivacy(mode)
    }
    
    func onSetRoomCoverUrl(_ url: String) {
        context.roomManager.onSetRoomCoverUrl(url)
    }
    
    func onStartLive(isJoinSelf: Bool, liveInfo: TUILiveInfo) {
        context.roomManager.onStartLive(isJoinSelf: isJoinSelf, liveInfo: liveInfo)
        context.userManager.onStartLive()
    }
    
    func onStopLive() {
        context.roomManager.onStopLive()
    }
    
    // Cross room
    func onCrossRoomConnectionTerminated() {
        context.coHostManager.onCrossRoomConnectionTerminated()
    }
    
    // Battle
    func onRequestBattle(battleId: String, battleUserList: [TUIBattleUser]) {
        context.battleManager.onRequestBattle(battleId: battleId, battleUserList: battleUserList)
    }
    
    func onResponseBattle() {
        context.battleManager.onResponseBattle()
    }
    
    func onBattleExited() {
        context.battleManager.onBattleExited()
    }
    
    // Other
    func fetchLiveInfo(roomId: String, onSuccess: @escaping ((_ liveInfo: TUILiveInfo)->()), onError: @escaping InternalErrorBlock) {
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
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await context.userManager.getUserInfo(userId: userId)
    }
    
    func onDisableSendingMessageBtnClicked(userId: String, isDisable: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.userManager.onDisableSendingMessageBtnClicked(userId: userId, isDisable: isDisable)
                DispatchQueue.main.async {
                    onSuccess()
                }
            } catch let err as InternalError {
                DispatchQueue.main.async {
                    onError(err)
                }
            }
        }
    }
    
    func onKickedOutBtnClicked(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.userManager.onKicedOutBtnClicked(userId: userId)
                DispatchQueue.main.async {
                    onSuccess()
                }
            } catch let err as InternalError {
                DispatchQueue.main.async {
                    onError(err)
                }
            }
        }
    }
    
    func onLockMediaStatusBtnClicked(userId: String, lockParams: TUISeatLockParams, onSuccess: @escaping TUISuccessBlock, onError: @escaping InternalErrorBlock) {
        Task {
            do {
                try await context.coGuestManager.onLockMediaStatusBtnClicked(userId: userId, lockParams: lockParams)
                DispatchQueue.main.async {
                    onSuccess()
                }
            } catch let err as InternalError {
                DispatchQueue.main.async {
                    onError(err)
                }
            }
        }
    }
}

// MARK: - Observer
extension AnchorManager {
    func onUserConnectionRejected(userId: String) {
        toastSubject.send(.takeSeatApplicationRejected)
        context.coGuestManager.onUserConnectionRejected(userId: userId)
    }
    
    func onUserConnectionTimeout(userId: String) {
        toastSubject.send(.takeSeatApplicationTimeout)
        context.coGuestManager.onUserConnectionTimeout(userId: userId)
    }
    
    func onKickedOffSeat() {
        toastSubject.send(.kickedOutOfSeat)
        context.coGuestManager.onKickedOffSeat()
    }
    
    func onConnectionUserListChanged(list: [TUIConnectionUser]) {
        context.coHostManager.onConnectionUserListChanged(list: list)
    }
}

// MARK: - Tools
extension AnchorManager {
    // State
    var roomState: AnchorRoomState {
        context.roomManager.roomState
    }
    var mediaState: AnchorMediaState {
        context.mediaManager.mediaState
    }
    var userState: AnchorUserState {
        context.userManager.userState
    }
    var coGuestState: AnchorCoGuestState {
        context.coGuestManager.coGuestState
    }
    var battleState: AnchorBattleState {
        context.battleManager.state
    }
    var coHostState: AnchorCoHostState {
        context.coHostManager.state
    }
    
    // Manager
    var coHostManager: AnchorCoHostManager {
        context.coHostManager
    }
    var battleManager: AnchorBattleManager {
        context.battleManager
    }
    var mediaManager: AnchorMediaManager {
        context.mediaManager
    }
    
    // Other
    func getDefaultRoomName() -> String {
        return context.roomManager.getDefaultRoomName()
    }
    
    func subscribeState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        if let sel = selector as? StateSelector<AnchorUserState, Value> {
            return context.userManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AnchorRoomState, Value> {
            return context.roomManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AnchorBattleState, Value> {
            return context.battleManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AnchorCoHostState, Value> {
            return context.coHostManager.subscribeCoHostState(sel)
        } else if let sel = selector as? StateSelector<AnchorMediaState, Value> {
            return context.mediaManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AnchorCoGuestState, Value> {
            return context.coGuestManager.subscribeState(sel)
        }
        assert(false, "Not impl")
        return Empty<Value, Never>().eraseToAnyPublisher()
    }
    
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        guard let provider = context.provider else { return Empty<Value, Never>().eraseToAnyPublisher() }
        return provider.subscribeCoreViewState(selector)
    }
}

extension AnchorManager {
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
    var coreBattleState: BattleState {
        context.coreBattleState
    }
}

extension AnchorManager.Context {
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
    var coreBattleState: BattleState {
        guard let provider = provider else { return BattleState() }
        return provider.getCoreViewState()
    }
}

fileprivate extension String {
    static let takeSeatApplicationRejected = internalLocalized("Take seat application has been rejected")
    static let takeSeatApplicationTimeout = internalLocalized("Take seat application timeout")
    static let kickedOutOfSeat = internalLocalized("Kicked out of seat by room owner")
}
