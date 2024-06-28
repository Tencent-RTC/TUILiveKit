//
//  MediaService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import Foundation
import RTCRoomEngine
import Combine
import TUICore

#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class MediaService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine?
    required init(roomEngine: TUIRoomEngine?) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func operateMicrophone(isOpened: Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            if isOpened {
                roomEngine.openLocalMicrophone(.default) {
                    promise(.success(true))
                } onError: { err, message in
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            } else {
                roomEngine.closeLocalMicrophone()
                promise(.success(false))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func muteLocalAudio(isMuted: Bool) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            if isMuted {
                roomEngine.muteLocalAudio()
                promise(.success(()))
            } else {
                roomEngine.unmuteLocalAudio {
                    promise(.success(()))
                } onError: { err, message in
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            }
        }
        .eraseToAnyPublisher()
    }
    
    func operateCamera(isOpened: Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            if isOpened {
                roomEngine.openLocalCamera(isFront: true, quality: .quality1080P) {
                    promise(.success(true))
                } onError: { err, message in
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            } else {
                roomEngine.closeLocalCamera()
                promise(.success(false))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func openLocalCamera(isFront: Bool) -> AnyPublisher<CameraDirection, InternalError> {
        return Future<CameraDirection, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.openLocalCamera(isFront: isFront, quality: .quality1080P) {
                promise(.success(isFront ? .front:.rear))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func updateVideoQuality(quality: TUIVideoQuality) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.updateVideoQuality(quality)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func updateAudioQuality(quality: TUIAudioQuality) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.updateAudioQuality(quality)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setLocalVideoView(view: UIView?) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.setLocalVideoView(view: view)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func stopLocalPreview() -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getTRTCCloud().stopLocalPreview()
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func switchCamera(isFrontCamera: Bool) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.switchCamera(frontCamera: isFrontCamera)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func switchMirror(enable:Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "switchMirror:[enable:\(enable)]")
            let params = TRTCRenderParams()
            params.mirrorType = enable ? .enable : .disable
            // TODO: here needs to change to engine`s API later
            roomEngine.getTRTCCloud().setLocalRenderParams(params)
            roomEngine.getTRTCCloud().setVideoEncoderMirror(enable)
            promise(.success(enable))
        }
        .eraseToAnyPublisher()
    }
    
    func muteAllRemoteAudio(isMute: Bool) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getTRTCCloud().muteAllRemoteAudio(isMute)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
