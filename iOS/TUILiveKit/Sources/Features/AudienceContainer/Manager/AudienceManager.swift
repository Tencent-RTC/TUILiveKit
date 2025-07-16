//
//  AudienceManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

typealias AudienceRoomStateUpdateClosure = (inout AudienceRoomState) -> Void
typealias AudienceUserStateUpdateClosure = (inout AudienceUserState) -> Void
typealias AudienceMediaStateUpdateClosure = (inout AudienceMediaState) -> Void
typealias AudienceCoGuestStateUpdateClosure = (inout AudienceCoGuestState) -> Void

protocol AudienceManagerProvider: NSObject {
    func getCoreViewState<T: State>() -> T
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never>
}

class AudienceManager {
    
    public let toastSubject = PassthroughSubject<String, Never>()
    public let floatWindowSubject = PassthroughSubject<Void, Never>()
    public let kickedOutSubject = PassthroughSubject<Void, Never>()
    
    static private let observerAudienceConfig = ObservableState<AudienceContainerConfig>(initialState: AudienceContainerConfig())
    static var audienceContainerConfig: AudienceContainerConfig {
        observerAudienceConfig.state
    }
    
    class Context {
        let service = AudienceService()
        weak var provider: AudienceManagerProvider?
        
        private(set) lazy var roomManager = AudienceRoomManager(context: self)
        private(set) lazy var userManager = AudienceUserManager(context: self)
        private(set) lazy var mediaManager = AudienceMediaManager(context: self)
        private(set) lazy var coGuestManager = AudienceCoGuestManager(context: self)
        
        private(set) lazy var battleManager = AudienceBattleManager(context: self)
        
        private(set) lazy var engineObserver = AudienceRoomEngineObserver(context: self)
        private(set) lazy var liveListObserver = AudienceLiveListObserver(context: self)
        private(set) lazy var liveLayoutObserver = AudienceLiveLayoutObserver(context: self)

        let toastSubject: PassthroughSubject<String, Never>
        let kickedOutSubject: PassthroughSubject<Void, Never>
        
        init(provider: AudienceManagerProvider, toastSubject: PassthroughSubject<String, Never>, kickedOutSubject: PassthroughSubject<Void, Never>) {
            self.toastSubject = toastSubject
            self.kickedOutSubject = kickedOutSubject
            self.provider = provider
            service.addEngineObserver(engineObserver)
            service.addLiveListManagerObserver(liveListObserver)
            service.addLiveLayoutObserver(liveLayoutObserver)
        }
        
        deinit {
            service.removeEngineObserver(engineObserver)
            service.removeLiveListManagerObserver(liveListObserver)
            service.removeLiveLayoutObserver(liveLayoutObserver)
        }
    }
    
    private let context: Context
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(provider: AudienceManagerProvider) {
        self.context = Context(provider: provider, toastSubject: toastSubject, kickedOutSubject: kickedOutSubject)
    }
}

// MARK: - AudienceConfig
extension AudienceManager {
    static func disableFeature(_ feature: AudienceViewFeature, isDisable: Bool) {
        observerAudienceConfig.update { config in
            switch feature {
                case .sliding:
                    config.disableSliding = isDisable
                case .floatWin:
                    config.disableHeaderFloatWin = isDisable
                case .liveData:
                    config.disableHeaderLiveData = isDisable
                case .visitorCnt:
                    config.disableHeaderVisitorCnt = isDisable
                case .coGuest:
                    config.disableFooterCoGuest = isDisable
            }
        }
    }
    
    static func subscribeAudienceConfig<Value>(_ selector: StateSelector<AudienceContainerConfig, Value>) -> AnyPublisher<Value, Never> {
        return observerAudienceConfig.subscribe(selector)
    }
}

// MARK: - Common
extension AudienceManager {
    func onError(_ error: InternalError) {
        toastSubject.send(error.localizedMessage)
    }
    
    func onCameraOpened(localVideoView: UIView) {
        context.mediaManager.onCameraOpened()
        context.mediaManager.setLocalVideoView(view: localVideoView)
    }
    
    func onReceiveGift(price: Int, senderUserId: String) {
        context.roomManager.onReceiveGift(price: price, senderUserId: senderUserId)
    }
}

// MARK: - Audience
extension AudienceManager {
    func prepareRoomIdBeforeEnterRoom(roomId: String) {
        context.roomManager.prepareRoomIdBeforeEnterRoom(roomId: roomId)
    }
    
    func onJoinLive(roomInfo: TUIRoomInfo) {
        context.roomManager.onJoinLive(roomInfo: roomInfo)
    }
    
    func onLeaveLive() {
        context.roomManager.onLeaveLive()
        context.userManager.onLeaveLive()
        context.mediaManager.onLeaveLive()
    }
    
    func onAudienceSliderCellInit(liveInfo: LiveInfo) {
        context.roomManager.onAudienceSliderCellInit(liveInfo: liveInfo)
    }
    
    func onStartRequestIntraRoomConnection() {
        context.coGuestManager.onStartRequestIntraRoomConnection()
    }
    
    func onRequestIntraRoomConnectionFailed() {
        context.coGuestManager.onRequestIntraRoomConnectionFailed()
    }
    
    func onStartCancelIntraRoomConnection() {
        context.coGuestManager.onStartCancelIntraRoomConnection()
    }
    
    func onCancelIntraRoomConnection() {
        context.coGuestManager.onCancelIntraRoomConnection()
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await context.userManager.getUserInfo(userId: userId)
    }
    
    func onBattleExited() {
        context.battleManager.onBattleExited()
    }
}

// MARK: - Observer
extension AudienceManager {
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
}

// MARK: - Tools
extension AudienceManager {
    // State
    var roomState: AudienceRoomState {
        context.roomManager.roomState
    }
    var mediaState: AudienceMediaState {
        context.mediaManager.mediaState
    }
    var userState: AudienceUserState {
        context.userManager.userState
    }
    var coGuestState: AudienceCoGuestState {
        context.coGuestManager.coGuestState
    }
    var battleState: AudienceBattleState {
        context.battleManager.state
    }
    
    // Manager
    var battleManager: AudienceBattleManager {
        context.battleManager
    }
    var mediaManager: AudienceMediaManager {
        context.mediaManager
    }
    
    // Other
    func subscribeState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        if let sel = selector as? StateSelector<AudienceUserState, Value> {
            return context.userManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AudienceRoomState, Value> {
            return context.roomManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AudienceBattleState, Value> {
            return context.battleManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AudienceMediaState, Value> {
            return context.mediaManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<AudienceCoGuestState, Value> {
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

extension AudienceManager {
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

extension AudienceManager.Context {
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
