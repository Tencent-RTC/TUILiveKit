//
//  LiveStreamManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/28.
//

import Combine
import RTCCommon
import RTCRoomEngine

protocol UpdateUserInfoDelegate: AnyObject {
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason)
    func onUserVideoStateChanged(userId: String, hasVideo: Bool, reason: TUIChangeReason)
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag)
}

class LiveStreamManager {
    public class Context {
        let service: LiveStreamService = LiveStreamService()

        let observers: LiveStreamObserverList = LiveStreamObserverList()
        
        lazy var roomEngineObserver = RoomEngineObserver(context: self)
        lazy var liveConnectionObserver = LiveConnectionObserver(context: self)
        lazy var liveBattleObserver = LiveBattleObserver(context: self)
        lazy var imObserver = IMObserver(context: self)
        lazy var liveLayoutObserver = LiveLayoutObserver(context: self)
        
        lazy var roomManager = RoomManager(context: self)
        lazy var coGuestManager = CoGuestManager(context: self)
        lazy var coHostManager = CoHostManager(context: self)
        lazy var battleManager = BattleManager(context: self)
        lazy var userManager = UserManager(context: self)
        lazy var mediaManager = MediaManager(context: self)
        lazy var layoutManager = LayoutManager(context: self)
    }
    
    public let context: Context
    
    init() {
        context = Context()
    }
    
    func addObserver(_ observer: ConnectionObserver) {
        context.observers.addObserver(observer)
    }
    
    func removerObserver(_ observer: ConnectionObserver) {
        context.observers.removeObserver(observer)
    }
    
    func addEngineObserver() {
        context.service.addRoomEngineObserver(context.roomEngineObserver)
        context.service.addLiveConnectionManagerObserver(context.liveConnectionObserver)
        context.service.addLiveBattleManagerObserver(context.liveBattleObserver)
        context.service.addImObserver(context.imObserver)
        context.service.addLiveLayoutManagerObserver(context.liveLayoutObserver)
    }
       
    func removeEngineObserver() {
        context.service.removeRoomEngineObserver(context.roomEngineObserver)
        context.service.removeLiveConnectionManagerObserver(context.liveConnectionObserver)
        context.service.removeLiveBattleManagerObserver(context.liveBattleObserver)
        context.service.removeImObserver(context.imObserver)
        context.service.removeLiveLayoutManagerObserver(context.liveLayoutObserver)
    }
    
    func addBattleObserver(_ observer: BattleObserver) {
        context.observers.addBattleObserver(observer)
    }
    
    func removerBattleObserver(_ observer: BattleObserver) {
        context.observers.removeBattleObserver(observer)
    }
    
    func asyncRunMainThread(_ successBlock: @escaping TUISuccessBlock) {
        DispatchQueue.main.async {
            successBlock()
        }
    }
    
    func asyncRunMainThread(_ errorBlock: @escaping TUIErrorBlock, _ code: TUIError, _ message: String) {
        DispatchQueue.main.async {
            errorBlock(code, message)
        }
    }
    
    func asyncRunMainThread(_ successBlock: @escaping TUIBattleRequestBlock, _ battleId: String, _ battleUserList: [TUIBattleUser]) {
        DispatchQueue.main.async {
            successBlock(battleId, battleUserList)
        }
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

// MARK: - Room API
extension LiveStreamManager {
    var roomState: RoomState {
        context.roomManager.roomState
    }
    
    func subscribeRoomState<Value>(_ selector: StateSelector<RoomState, Value>) -> AnyPublisher<Value, Never> {
        return context.roomManager.observerState.subscribe(selector)
    }
    
    func startLive(roomInfo: TUIRoomInfo, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamCreateRoom)
        addEngineObserver()
        Task {
            do {
                let roomInfo = try await context.roomManager.startLive(roomInfo: roomInfo)
                DispatchQueue.main.async {
                    onSuccess(roomInfo)
                }
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func stopLive(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamDestroyRoom)
        removeEngineObserver()
        Task {
            do {
                try await context.roomManager.stopLive()
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func joinLive(roomId: String, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamJoinRoom)
        addEngineObserver()
        Task {
            do {
                let roomInfo = try await context.roomManager.joinLive(roomId: roomId)
                DispatchQueue.main.async {
                    onSuccess(roomInfo)
                }
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func leaveLive(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamLeaveRoom)
        removeEngineObserver()
        Task {
            do {
                try await context.roomManager.leaveLive()
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
}

// MARK: - CoGuest API
extension LiveStreamManager {
    var coGuestState: CoGuestState {
        context.coGuestManager.coGuestState
    }
    
    func subscribeCoGuestState<Value>(_ selector: StateSelector<CoGuestState, Value>) -> AnyPublisher<Value, Never> {
        return context.coGuestManager.observerState.subscribe(selector)
    }
    
    func isCoGuestEnable() -> Bool {
        return context.coGuestManager.isEnable()
    }
    
    func enableAutoOpenCameraOnSeated(enable: Bool) {
        context.coGuestManager.enableAutoOpenCameraOnSeated(enable: enable)
    }
    
    func requestIntraRoomConnection(userId: String, timeOut: Int, openCamera: Bool, 
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRequestIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.requestIntraRoomConnection(userId: userId, timeOut: timeOut, openCamera: openCamera) { [weak self] in
                    self?.asyncRunMainThread(onSuccess)
                }
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func cancelIntraRoomConnection(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamCancelIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.cancelIntraRoomConnection(userId: userId)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func respondIntraRoomConnection(userId: String, isAccepted: Bool,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRespondIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.respondIntraRoomConnection(userId: userId, isAccepted: isAccepted)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func disconnectByAdamin(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamDisconnectUser)
        Task {
            do {
                try await context.coGuestManager.disconnectByAdmin(userId: userId)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func disconnectBySelf() {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamTerminateIntraRoomConnection)
        Task {
            try await context.coGuestManager.disconnectBySelf()
        }
    }
}

// MARK: - CoHost API
extension LiveStreamManager {
    var coHostState: CoHostState {
        context.coHostManager.coHostState
    }
    
    func subscribeCoHostState<Value>(_ selector: StateSelector<CoHostState, Value>) -> AnyPublisher<Value, Never> {
        return context.coHostManager.observerState.subscribe(selector)
    }
    
    func isCoHostEnable() -> Bool {
        return context.coHostManager.isEnable()
    }
    
    func requestConnection(roomId: String, timeOut: Int,
                           onSuccess: @escaping ((TUIConnectionCode?) -> ()), onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRequestCrossRoomConnection)
        Task {
            do {
                let code = try await context.coHostManager.requestConnection(roomId: roomId, timeout: timeOut)
                DispatchQueue.main.async {
                    onSuccess(code)
                }
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func cancelRequest(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamCancelCrossRoomConnection)
        Task {
            do {
                try await context.coHostManager.cancelRequest(roomId: roomId)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func respondToCrossRoomConnection(roomId: String, isAccepted: Bool,
                                      onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRespondCrossRoomConnection)
        Task {
            do {
                if isAccepted {
                    try await context.coHostManager.accept(roomId: roomId)
                } else {
                    try await context.coHostManager.reject(roomId: roomId)
                }
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func disconnect() {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamTerminateCrossRoomConnection)
        Task {
            try await context.coHostManager.disconnect()
        }
    }
}

// MARK: - Battle API
extension LiveStreamManager {
    var battleState: BattleState {
        context.battleManager.battleState
    }
    
    func subscribeBattleState<Value>(_ selector: StateSelector<BattleState, Value>) -> AnyPublisher<Value, Never> {
        return context.battleManager.observerState.subscribe(selector)
    }
    
    func requestBattle(config: TUIBattleConfig, userIdList: [String],
                       timeout: TimeInterval, onSuccess: @escaping TUIBattleRequestBlock, onError: @escaping TUIErrorBlock) {
        Task {
            do {
                let (battleId, battleUserList) = try await context.battleManager.requestBattle(config: config, userIdList: userIdList, timeout: timeout)
                asyncRunMainThread(onSuccess, battleId, battleUserList)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }

    func cancelBattle(battleId: String, userIdList: [String], onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        Task {
            do {
                try await context.battleManager.cancelBattle(battleId: battleId, userIdList: userIdList)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }

    func respondToBattle(battleId: String, isAccepted: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        Task {
            do {
                if isAccepted {
                    try await context.battleManager.acceptBattle(battleId: battleId)
                } else {
                    try await context.battleManager.rejectBattle(battleId: battleId)
                }
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }

    func terminateBattle(battleId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        Task {
            do {
                try await context.battleManager.exitBattle(battleId: battleId)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
}

// MARK: - User API
extension LiveStreamManager {
    var userState: UserState {
        context.userManager.userState
    }
    
    func subscribeUserState<Value>(_ selector: StateSelector<UserState, Value>) -> AnyPublisher<Value, Never> {
        return context.userManager.observerState.subscribe(selector)
    }
}

// MARK: - Media API
extension LiveStreamManager {
    var mediaState: MediaState {
        context.mediaManager.mediaState
    }
    
    func subscribeMediaState<Value>(_ selector: StateSelector<MediaState, Value>) -> AnyPublisher<Value, Never> {
        return context.mediaManager.observerState.subscribe(selector)
    }
    
    func startLocalCamera(useFrontCamera: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamStartCamera)
        Task {
            do {
                try await context.mediaManager.openLocalCamera(useFrontCamera: useFrontCamera)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func startMicrophone(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamStartMicrophone)
        Task {
            do {
                try await context.mediaManager.openLocalMicrophone()
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func muteMicrophone(mute: Bool) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamMuteMicrophone)
        context.mediaManager.muteLocalAudio(mute: mute)
    }
    
    func stopCamera() {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamStopCamera)
        context.mediaManager.closeLocalCamera()
    }
    
    func stopMicrophone() {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamStopMicrophone)
        context.mediaManager.closeLocalMicrophone()
    }
    
    func setLocalVideoView(view: UIView?) {
        context.mediaManager.setLocalVideoView(view: view)
    }
    
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, view: UIView) {
        context.mediaManager.setRemoteVideoView(userId: userId, streamType: streamType, videoView: view)
    }
    
    func startPreloadVideoStream(roomId: String, isMuteAudio: Bool, view: UIView,
                                 onPlaying: @escaping TUIPlayOnPlayingBlock,
                                 onLoading: @escaping TUIPlayOnLoadingBlock,
                                 onError: @escaping TUIPlayOnErrorBlock) {
        context.mediaManager.startPreloadVideoStream(roomId: roomId, isMuteAudio: isMuteAudio, view: view,
                                                     onPlaying: onPlaying, onLoading: onLoading, onError: onError)
    }
    
    func stopPreloadVideoStream(roomId: String) {
        context.mediaManager.stopPreloadVideoStream(roomId: roomId)
    }
}

// MARK: - Layout API
extension LiveStreamManager {
    var layoutState: LayoutState {
        context.layoutManager.layoutState
    }
    
    func subscribeLayoutState<Value>(_ selector: StateSelector<LayoutState, Value>) -> AnyPublisher<Value, Never> {
        return context.layoutManager.observerState.subscribe(selector)
    }
    
    func updateVideoLayout(layout: VideoLayoutInfo?) {
        context.layoutManager.updateVideoLayout(layout: layout)
    }
}
