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
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func operateMicrophone(isOpened: Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
            if isOpened {
                LiveKitLog.info("\(#file)", "\(#line)","openLocalMicrophone")
                roomEngine.openLocalMicrophone(.default) {
                    LiveKitLog.info("\(#file)", "\(#line)","openLocalMicrophone:[onSuccess]")
                    promise(.success(true))
                } onError: { err, message in
                    LiveKitLog.error("\(#file)", "\(#line)","openLocalMicrophone:[onError:[error:\(err) message:\(message)]]")
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            } else {
                LiveKitLog.info("\(#file)", "\(#line)","closeLocalMicrophone")
                roomEngine.closeLocalMicrophone()
                promise(.success(false))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func muteLocalAudio(isMuted: Bool) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            if isMuted {
                LiveKitLog.info("\(#file)", "\(#line)","muteLocalAudio")
                roomEngine.muteLocalAudio()
                promise(.success(()))
            } else {
                LiveKitLog.info("\(#file)", "\(#line)","unmuteLocalAudio")
                roomEngine.unmuteLocalAudio {
                    LiveKitLog.info("\(#file)", "\(#line)","unmuteLocalAudio:[onSuccess]")
                    promise(.success(()))
                } onError: { err, message in
                    LiveKitLog.error("\(#file)", "\(#line)","unmuteLocalAudio:[onError:[error:\(err) message:\(message)]]")
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            }
        }
        .eraseToAnyPublisher()
    }
    
    func operateCamera(isOpened: Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
            if isOpened {
                LiveKitLog.info("\(#file)", "\(#line)","openLocalCamera:[isFront:1]")
                roomEngine.openLocalCamera(isFront: true, quality: .quality1080P) {
                    LiveKitLog.info("\(#file)", "\(#line)","openLocalCamera:[onSuccess]")
                    promise(.success(true))
                } onError: { err, message in
                    LiveKitLog.error("\(#file)", "\(#line)","openLocalCamera:[onError:[error:\(err) message:\(message)]]")
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            } else {
                LiveKitLog.info("\(#file)", "\(#line)","closeLocalCamera")
                roomEngine.closeLocalCamera()
                promise(.success(false))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func openLocalCamera(isFront: Bool) -> AnyPublisher<CameraDirection, InternalError> {
        return Future<CameraDirection, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","openLocalCamera:[isFront:\(isFront),quality:\(TUIVideoQuality.quality1080P.rawValue)]")
            roomEngine.openLocalCamera(isFront: isFront, quality: .quality1080P) {
                LiveKitLog.info("\(#file)", "\(#line)","openLocalCamera:[onSuccess]")
                promise(.success(isFront ? .front:.rear))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)","openLocalCamera:[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func updateVideoQuality(quality: TUIVideoQuality) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","updateVideoQuality:[quality:\(quality.rawValue)]")
            roomEngine.updateVideoQuality(quality)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func updateAudioQuality(quality: TUIAudioQuality) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","updateAudioQuality:[quality:\(quality.rawValue)]")
            roomEngine.updateAudioQuality(quality)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func setLocalVideoView(view: UIView?) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","setLocalVideoView:[view:\(String(describing: view))]")
            roomEngine.setLocalVideoView(view: view)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func stopLocalPreview() -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","stopLocalPreview")
            roomEngine.getTRTCCloud().stopLocalPreview()
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func switchCamera(isFrontCamera: Bool) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","switchCamera[frontCamera:\(isFrontCamera)]")
            roomEngine.switchCamera(frontCamera: isFrontCamera)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func switchMirror(enable:Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
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
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "muteAllRemoteAudio:[isMute:\(isMute)]")
            roomEngine.getTRTCCloud().muteAllRemoteAudio(isMute)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
