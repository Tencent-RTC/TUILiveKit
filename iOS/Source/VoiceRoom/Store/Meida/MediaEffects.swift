//
//  MediaEffects.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import Combine
import Foundation

class MediaEffects: Effects {
    typealias Environment = ServiceCenter
    
    let operateMicrophone = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.operateMicrophone)
            .flatMap{ action in
                environment.mediaService.operateMicrophone(isOpened: action.payload)
                    .map { isOpened in
                        if isOpened {
                            return MediaActions.microphoneOpened()
                        } else {
                            return MediaActions.microphoneClosed()
                        }
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
                
            }
            .eraseToAnyPublisher()
    }
    
    let muteLocalAudio = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: MediaActions.operateLocalAudioMute)
            .sink { action in
                environment.mediaService.muteLocalAudio(isMuted: action.payload)
                    .sink { completion in
                        switch completion {
                            case let .failure(error):
                            environment.store?.dispatch(action: ViewActions.toast(payload: ToastInfo(message: error.localizedMessage)))
                                break
                            default:
                                break
                        }
                    } receiveValue: {
                        print("mute operate succeed.")
                    }
                    .cancel()
            }
    }
}
