//
//  MediaManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/25.
//

import RTCCommon
import RTCRoomEngine

class MediaManager {
    let observerState = ObservableState<MediaState>(initialState: MediaState())
    var mediaState: MediaState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
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
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
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
        service.setLocalVideoView(view)
    }
    
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, videoView: UIView) {
        service.setRemoteVideoView(userId: userId, streamType: streamType, videoView: videoView)
    }
    
    func startPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) async throws -> String {
        do {
            let result = try await service.startPlayRemoteVideo(userId: userId, streamType: streamType) { _ in }
            return result
        } catch let LiveStreamCoreError.playVideoError(userId, code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.playVideoError(userId: userId, code: code, message: message)
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
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
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
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
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
}

// MARK: - Callback from other manager
extension MediaManager {
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
    }
}

// MARK: - Private
extension MediaManager {
    private func initLivingConfig() {
        service.enableGravitySensor(enable: true)
        service.setVideoResolutionMode(.portrait)
        service.setBeautyStyle(.smooth)
    }
    
    private func modifyMediaState<T>(value: T, keyPath: WritableKeyPath<MediaState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { mediaState in
            mediaState[keyPath: keyPath] = value
        }
    }
}
