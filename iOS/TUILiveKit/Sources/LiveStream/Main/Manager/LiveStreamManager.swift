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
import TUILiveComponent

typealias LSRoomStateUpdateClosure = (inout LSRoomState) -> Void
typealias LSUserStateUpdateClosure = (inout LSUserState) -> Void
typealias LSMediaStateUpdateClosure = (inout LSMediaState) -> Void
typealias LSCoGuestStateUpdateClosure = (inout LSCoGuestState) -> Void

typealias InternalErrorBlock = (_ error: InternalError) -> Void

protocol LiveStreamManagerProvider: NSObject {
    func getCoreViewState<T: State>() -> T
    func subscribeCoreViewState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never>
}

class LiveStreamManager {
    
    public let toastSubject = PassthroughSubject<String, Never>()
    public let likeSubject = PassthroughSubject<Void, Never>()
    public let floatWindowSubject = PassthroughSubject<Void, Never>()
    public let kickedOutSubject = PassthroughSubject<Void, Never>()
    
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
        let kickedOutSubject: PassthroughSubject<Void, Never>
        
        init(provider: LiveStreamManagerProvider, toastSubject: PassthroughSubject<String, Never>, kickedOutSubject: PassthroughSubject<Void, Never>) {
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
    
    init(provider: LiveStreamManagerProvider) {
        self.context = Context(provider: provider, toastSubject: toastSubject, kickedOutSubject: kickedOutSubject)
    }
}

// MARK: - Common
extension LiveStreamManager {
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

// MARK: - Anchor
extension LiveStreamManager {
    func prepareLiveInfoBeforeEnterRoom(liveInfo: TUILiveInfo) {
        context.roomManager.prepareLiveInfoBeforeEnterRoom(liveInfo: liveInfo)
    }
    
    func prepareRoomIdBeforeEnterRoom(roomId: String) {
        context.roomManager.prepareRoomIdBeforeEnterRoom(roomId: roomId)
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
    
    func onStartLive(isJoinSelf: Bool, roomInfo: TUIRoomInfo) {
        context.roomManager.onStartLive(isJoinSelf: isJoinSelf, roomInfo: roomInfo)
        context.userManager.onStartLive()
    }
    
    func onStopLive() {
        context.roomManager.onStopLive()
    }
    
    func onCoHostConnectUserChanged(connectUserList: [TUIConnectionUser]) {
        context.mediaManager.changeVideoEncParams(encType: connectUserList.count > 1 ? .small : .big)
    }
    
    func onCoGuestConnectUserChanged(connectUserList: [TUIUserInfo]) {
        context.mediaManager.changeVideoEncParams(encType: connectUserList.count > 1 ? .small : .big)
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

// MARK: - Audience
extension LiveStreamManager {
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
    
    func onCoGuestStatusChanged(status: LSCoGuestState.CoGuestStatus) {
        if status == .linking {
            context.mediaManager.changeVideoEncParams(encType: .small)
        }
    }
}

// MARK: - Observer
extension LiveStreamManager {
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
extension LiveStreamManager {
    // State
    var roomState: LSRoomState {
        context.roomManager.roomState
    }
    var mediaState: LSMediaState {
        context.mediaManager.mediaState
    }
    var userState: LSUserState {
        context.userManager.userState
    }
    var coGuestState: LSCoGuestState {
        context.coGuestManager.coGuestState
    }
    var battleState: LSBattleState {
        context.battleManager.state
    }
    var coHostState: LSCoHostState {
        context.coHostManager.state
    }
    
    // Manager
    var coHostManager: LSCoHostManager {
        context.coHostManager
    }
    var battleManager: LSBattleManager {
        context.battleManager
    }
    var mediaManager: LSMediaManager {
        context.mediaManager
    }
    
    // Other
    func getDefaultRoomName() -> String {
        return context.roomManager.getDefaultRoomName()
    }
    
    func subscribeState<State, Value>(_ selector: StateSelector<State, Value>) -> AnyPublisher<Value, Never> {
        if let sel = selector as? StateSelector<LSUserState, Value> {
            return context.userManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<LSRoomState, Value> {
            return context.roomManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<LSBattleState, Value> {
            return context.battleManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<LSCoHostState, Value> {
            return context.coHostManager.subscribeCoHostState(sel)
        } else if let sel = selector as? StateSelector<LSMediaState, Value> {
            return context.mediaManager.subscribeState(sel)
        } else if let sel = selector as? StateSelector<LSCoGuestState, Value> {
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
    var coreBattleState: BattleState {
        context.coreBattleState
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
    var coreBattleState: BattleState {
        guard let provider = provider else { return BattleState() }
        return provider.getCoreViewState()
    }
}

extension LiveStreamManager: GiftListPanelProvider {
    func getAnchorInfo() -> GiftUser {
        let owner = coreRoomState.ownerInfo
        let giftUser = GiftUser(userId: owner.userId, name: owner.userName, avatarUrl: owner.avatarUrl)
        return giftUser
    }
}

fileprivate extension String {
    static let takeSeatApplicationRejected = internalLocalized("Take seat application has been rejected")
    static let takeSeatApplicationTimeout = internalLocalized("Take seat application timeout")
    static let kickedOutOfSeat = internalLocalized("Kicked out of seat by room owner")
}
