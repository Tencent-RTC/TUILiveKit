//
//  MediaService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import Foundation
import RTCRoomEngine

import Combine

class MediaService {
    private let engine = TUIRoomEngine.sharedInstance()
    private var isMicrophoneOpened: Bool = false
    
    func operateMicrophone(isOpened: Bool) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
            if isOpened {
                self.engine.openLocalMicrophone(.default) {
                    promise(.success(true))
                } onError: { err, message in
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            } else {
                self.engine.closeLocalMicrophone()
                promise(.success(false))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func muteLocalAudio(isMuted: Bool) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            if isMuted {
                self.engine.muteLocalAudio()
                promise(.success(()))
            } else {
                self.engine.unmuteLocalAudio {
                    promise(.success(()))
                } onError: { err, message in
                    let error = InternalError(error: err, message: message)
                    promise(.failure(error))
                }
            }
        }
        .eraseToAnyPublisher()
    }
}
