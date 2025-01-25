//
//  MediaManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/25.
//

import RTCCommon
import RTCRoomEngine
import TUICore
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

public class MediaManager {
    let observerState = ObservableState<MediaState>(initialState: MediaState())
    public var mediaState: MediaState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    private var localVideoViewObservation: NSKeyValueObservation? = nil
    private var preloadUserId: String? = nil
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
        initVideoAdvanceSettings()
    }
    
    deinit {
        unInitVideoAdvanceSettings()
    }
    
    func openLocalCamera(useFrontCamera: Bool) async throws {
        guard let context = context else {
            throw LiveStreamCoreError.error(code: .failed, message: "context of manager is empty")
        }
        if mediaState.isFrontCamera != useFrontCamera && mediaState.isCameraOpened {
            switchCamera()
            return
        }
        modifyMediaState(value: useFrontCamera, keyPath: \MediaState.isFrontCamera)
        
        if mediaState.isCameraOpened {
            throw LiveStreamCoreError.error(code: .repeatOperation, message: TUIError.repeatOperation.lcDescription)
        }
        
        do {
            try await context.service.openLocalCamera(isFront: useFrontCamera, quality: .quality1080P)
            initLivingConfig()
            modifyMediaState(value: true, keyPath: \MediaState.isCameraOpened, isPublished: true)
            // Default automatic mode: Enable mirroring if the front camera is in use, disable mirroring if the rear camera is in use (only for mobile devices)
            enableMirror(useFrontCamera, isPublished: true)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","openLocalCamera:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func closeLocalCamera() {
        service.closeLocalCamera()
        modifyMediaState(value: false, keyPath: \MediaState.isCameraOpened, isPublished: true)
    }
    
    func setLocalVideoView(view: UIView?) {
        if let localView = view {
            subscribeLocalVideoView(localView)
        } else {
            unsubscribeLocalVideoView()
        }
        service.setLocalVideoView(view)
    }
    
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, videoView: UIView) {
        service.setRemoteVideoView(userId: userId, streamType: streamType, videoView: videoView)
    }
    
    func startPlayRemoteVideo(userId: String,
                              streamType: TUIVideoStreamType,
                              onPlaying: TUIPlayOnPlayingBlock? = nil,
                              onLoading: TUIPlayOnLoadingBlock? = nil,
                              onError: TUIPlayOnErrorBlock? = nil) {
        service.startPlayRemoteVideo(userId: userId, streamType: streamType) { userId in
            onPlaying?(userId)
        } onLoading: { userId in
            onLoading?(userId)
        } onError: { userId, code, message in
            onError?(userId, code, message)
            LiveStreamLog.error("\(#file)","\(#line)","startPlayRemoteVideo:[code:\(code),message:\(message)]")
        }
    }
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) {
        service.stopPlayRemoteVideo(userId: userId, streamType: streamType)
    }
    
    func openLocalMicrophone() async throws {
        do {
            try await unMuteLocalAudio()
            try await service.openLocalMicrophone()
            modifyMediaState(value: true, keyPath: \MediaState.isMicrophoneOpened)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","openLocalMicrophone:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func closeLocalMicrophone() {
        service.closeLocalMicrophone()
        modifyMediaState(value: false, keyPath: \MediaState.isMicrophoneOpened)
    }
    
    func unMuteLocalAudio() async throws {
        do {
            try await service.unMuteLocalAudio()
            modifyMediaState(value: false, keyPath: \MediaState.isMicrophoneMuted)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","unMuteLocalAudio:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func muteLocalAudio(mute: Bool) {
        if mute {
            service.muteLocalAudio()
            modifyMediaState(value: true, keyPath: \MediaState.isMicrophoneMuted)
        } else {
            Task {
                try await unMuteLocalAudio()
            }
        }
    }
    
    func switchCamera() {
        let isFrontCamera = mediaState.isFrontCamera
        service.switchCamera(isFrontCamera: !isFrontCamera)
        observerState.update(isPublished: false) { mediaState in
            mediaState.isFrontCamera = !isFrontCamera
        }
    }
    
    func startPreloadVideoStream(roomId: String, isMuteAudio: Bool, view: UIView,
                                 onPlaying: @escaping TUIPlayOnPlayingBlock,
                                 onLoading: @escaping TUIPlayOnLoadingBlock,
                                 onError: @escaping TUIPlayOnErrorBlock) {
        service.startPreloadVideoStream(roomId: roomId, isMuteAudio: isMuteAudio, view: view) { [weak self] userId in
            guard let self = self else { return }
            preloadUserId = userId
            onPlaying(userId)
        } onLoading: { userId in
            onLoading(userId)
        } onError: { userId, code, message in
            onError(userId, code, message)
        }
    }
    
    func stopPreloadVideoStream(roomId: String) {
        service.stopPreloadVideoStream(roomId: roomId)
    }
    
    func muteRemoteAudioStream(_ userId: String, isMute: Bool) {
        service.muteRemoteAudioStream(userId, isMute: isMute)
    }
}

// MARK: - Video Advance Setting
extension MediaManager {
    
    public func updateVideoEncParams(_ params: TUIRoomVideoEncoderParams) {
        service.updateVideoQualityEx(streamType: .cameraStream, params: params)
        modifyMediaState(value: params, keyPath: \MediaState.videoEncParams.currentEnc)
    }
    
    public func changeVideoEncParams(encType: VideoEncParams.VideoEncType) {
        switch encType {
        case .big:
            service.updateVideoQualityEx(streamType: .cameraStream, params: mediaState.videoEncParams.big)
        case .small:
            service.updateVideoQualityEx(streamType: .cameraStream, params: mediaState.videoEncParams.small)
        }
        modifyMediaState(value: encType, keyPath: \MediaState.videoEncParams.currentEncType)
    }
    
    public func enableMirror(_ isMirror: Bool, isPublished: Bool = false) {
        let params = TRTCRenderParams()
        params.mirrorType = isMirror ? .enable : .disable
        service.getTRTCCloud().setLocalRenderParams(params)
        service.getTRTCCloud().setVideoEncoderMirror(isMirror)
        modifyMediaState(value: isMirror, keyPath: \MediaState.isMirrorEnabled, isPublished: isPublished)
    }
    
    public func enableAdvancedVisible(_ visible: Bool) {
        observerState.update { state in
            state.videoAdvanceSettings.isVisible = visible
        }
    }
    
    public func enableUltimate(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableUltimate,
                            param: ["enable" : NSNumber(value: enable)])
        modifyMediaState(value: enable, keyPath: \MediaState.videoAdvanceSettings.isUltimateEnabled)
        if enable {
            enableBFrame(false)
        }
    }
    
    public func enableBFrame(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableBFrame,
                            param: ["enable" : NSNumber(value: enable)])
        modifyMediaState(value: enable, keyPath: \MediaState.videoAdvanceSettings.isBFrameEnabled)
    }
    
    public func enableH265(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableH265,
                            param: ["enable" : NSNumber(value: enable)])
        modifyMediaState(value: enable, keyPath: \MediaState.videoAdvanceSettings.isH265Enabled)
    }
    
    public func enableHDR(_ renderType: HDRRenderType) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableHDR,
                            param: ["renderType" : NSNumber(value: renderType.rawValue)])
        modifyMediaState(value: renderType, keyPath: \MediaState.videoAdvanceSettings.hdrRenderType)
    }
    
    private func subscribeLocalVideoView(_ view: UIView) {
        localVideoViewObservation = view.observe(\.bounds, options: [.new]) { [weak self] (_, change) in
            guard let self = self else { return }
            guard let newFrame = change.newValue else { return }
            if floor(newFrame.width)/ScreenWidth > 0.5 {
                self.changeVideoEncParams(encType: .big)
            } else {
                self.changeVideoEncParams(encType: .small)
            }
        }
    }
    
    private func unsubscribeLocalVideoView() {
        if let observation = localVideoViewObservation {
            observation.invalidate()
        }
        localVideoViewObservation = nil
    }
    
    private func initVideoAdvanceSettings() {
        enableUltimate(true)
        enableH265(true)
    }
    
    private func unInitVideoAdvanceSettings() {
        enableUltimate(false)
        enableH265(false)
        
        let param = TUIRoomVideoEncoderParams()
        param.videoResolution = .quality1080P
        param.resolutionMode = .portrait
        param.fps = 30
        param.bitrate = 6000
        updateVideoEncParams(param)
    }
}

// MARK: - Callback from other manager
extension MediaManager {
    func onJoinLiveSuccess() {
        guard let preloadUserId = preloadUserId else { return }
        muteRemoteAudioStream(preloadUserId, isMute: false)
    }
    
    func onSelfAudioStateChanged(hasAudio: Bool) {
        modifyMediaState(value: !hasAudio, keyPath: \MediaState.isMicrophoneMuted)
        if hasAudio {
            modifyMediaState(value: true, keyPath: \MediaState.isMicrophoneOpened)
        }
    }
    
    func onSelfVideoStateChanged(hasVideo: Bool) {
        modifyMediaState(value: hasVideo, keyPath: \MediaState.isCameraOpened, isPublished: true)
    }
    
    func onLeaveRoom() {
        modifyMediaState(value: false, keyPath: \MediaState.hasMicrophonePermission)
        modifyMediaState(value: false, keyPath: \MediaState.isMicrophoneOpened)
        modifyMediaState(value: true, keyPath: \MediaState.isMicrophoneMuted)
        modifyMediaState(value: false, keyPath: \MediaState.isCameraOpened, isPublished: true)
        modifyMediaState(value: true, keyPath: \MediaState.isFrontCamera)
        unInitVideoAdvanceSettings()
    }
}

// MARK: - Private
extension MediaManager {
    private func initLivingConfig() {
        service.enableGravitySensor(enable: true)
        service.setVideoResolutionMode(.portrait)
    }
    
    private func modifyMediaState<T>(value: T, keyPath: WritableKeyPath<MediaState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { mediaState in
            mediaState[keyPath: keyPath] = value
        }
    }
}

// MARK: - Video Advance API Extension
fileprivate extension String {
    
    static let TUICore_VideoAdvanceService = "TUICore_VideoAdvanceService"
    
    static let TUICore_VideoAdvanceService_EnableUltimate = "TUICore_VideoAdvanceService_EnableUltimate"
    static let TUICore_VideoAdvanceService_EnableH265 = "TUICore_VideoAdvanceService_EnableH265"
    static let TUICore_VideoAdvanceService_EnableHDR = "TUICore_VideoAdvanceService_EnableHDR"
    static let TUICore_VideoAdvanceService_EnableBFrame = "TUICore_VideoAdvanceService_EnableBFrame"
}
