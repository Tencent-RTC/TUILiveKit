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
        
        weak var delegate: UpdateUserInfoDelegate?
        
        lazy var roomEngineObserver = RoomEngineObserver(context: self)
        lazy var liveConnectionObserver = LiveConnectionObserver(context: self)
        lazy var imObserver = IMObserver(context: self)
        lazy var liveLayoutObserver = LiveLayoutObserver(context: self)
        lazy var roomManager = RoomManager(context: self)
        lazy var coGuestManager = CoGuestManager(context: self)
        lazy var coHostManager = CoHostManager(context: self)
        lazy var userManager = UserManager(context: self)
        lazy var mediaManager = MediaManager(context: self)
        lazy var viewManager = ViewManager(context: self)
    }
    
    public let context: Context
    
    init() {
        context = Context()
        context.service.addRoomEngineObserver(context.roomEngineObserver)
        context.service.addLiveConnectionManagerObserver(context.liveConnectionObserver)
        context.service.addImObserver(context.imObserver)
        context.service.addLiveLayoutManagerObserver(context.liveLayoutObserver)
    }
    
    func addObserver(_ observer: ConnectionObserver) {
        context.observers.addObserver(observer)
    }
    
    func removerObserver(_ observer: ConnectionObserver) {
        context.observers.removeObserver(observer)
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
    
    deinit {
        debugPrint("deinit:\(self)")
        context.service.removeRoomEngineObserver(context.roomEngineObserver)
        context.service.removeLiveConnectionManagerObserver(context.liveConnectionObserver)
        context.service.removeImObserver(context.imObserver)
        context.service.removeLiveLayoutManagerObserver(context.liveLayoutObserver)
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
    
    func applyToConnection(timeOut: Int, onSendRequestSuccess: (() -> Void)?) async throws -> TakeSeatResult {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRequestIntraRoomConnection)
        return try await context.coGuestManager.applyToConnection(timeOut: timeOut, onSendRequestSuccess: onSendRequestSuccess)
    }
    
    func inviteGuestToConnection(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRequestIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.inviteGuestToConnection(userId: userId) { [weak self] in
                    self?.asyncRunMainThread(onSuccess)
                }
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func cancelGuestApplication(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamCancelIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.cancelGuestApplication()
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func cancelInviteApplication(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamCancelIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.cancelInviteApplication(userId: userId)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func respondGuestApplication(userId: String, isAccepted: Bool,
                                 onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRespondIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.respondGuestApplication(userId: userId, isAgree: isAccepted)
                asyncRunMainThread(onSuccess)
            } catch let LiveStreamCoreError.error(code, message) {
                asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    func respondGuestInvitation(isAgree: Bool,
                                     onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRespondIntraRoomConnection)
        Task {
            do {
                try await context.coGuestManager.respondGuestInvitation(isAgree: isAgree)
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
                           onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamRequestCrossRoomConnection)
        Task {
            do {
                try await context.coHostManager.requestConnection(roomId: roomId, timeout: timeOut)
                asyncRunMainThread(onSuccess)
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
    
    func startPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) {
        Task {
            let _ = try await context.mediaManager.startPlayRemoteVideo(userId: userId, streamType: streamType)
        }
    }
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) {
        context.mediaManager.stopPlayRemoteVideo(userId: userId, streamType: streamType)
    }
}

// MARK: - View API
extension LiveStreamManager {
    var viewState: ViewState {
        context.viewManager.viewState
    }
    
    func subscribeViewState<Value>(_ selector: StateSelector<ViewState, Value>) -> AnyPublisher<Value, Never> {
        return context.viewManager.observerState.subscribe(selector)
    }
    
    func getLocalLiveView() -> LiveStreamView {
        return context.viewManager.getLocalLiveView()
    }
    
    func clearLocalLiveView() {
        context.viewManager.clearLocalLiveView()
    }
    
    func removeRemoteView(userId: String) {
        context.viewManager.removeRemoteView(userId: userId)
    }
    
    func getRemoteLiveViewByUserId(userId: String) -> LiveStreamView {
        return context.viewManager.getRemoteLiveViewByUserId(userId: userId)
    }
    
    func updateVideoLayout(layout: VideoLayoutInfo?) {
        context.viewManager.updateVideoLayout(layout: layout)
    }
    
    func setLayoutMode(layoutMode: LayoutMode, layoutJson: String? = nil) {
        LCDataReporter.reportEventData(event: .methodCallLiveStreamSetLayoutMode)
        context.viewManager.setLayoutMode(layoutMode: layoutMode, layoutJson: layoutJson)
    }
}
