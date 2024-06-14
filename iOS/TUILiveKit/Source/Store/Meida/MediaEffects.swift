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
    
    let operateCamera = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.operateCamera)
            .flatMap{ action in
                environment.mediaService.operateCamera(isOpened: action.payload)
                    .map { isOpened in
                        if isOpened {
                            return MediaActions.cameraOpened(payload: .front)
                        } else {
                            return MediaActions.cameraClosed()
                        }
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
                
            }
            .eraseToAnyPublisher()
    }
    
    let switchCamera = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.switchCamera)
            .flatMap{ action in
                environment.mediaService.switchCamera(isFrontCamera: action.payload == .front)
                .map {
                    if action.payload  == .front{
                        return MediaActions.frontCameraOpened()
                    } else {
                        return MediaActions.rearCameraOpened()
                    }
                }
            }
            .eraseToAnyPublisher()
    }
    
    let muteLocalAudio = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: MediaActions.operateMicrophoneMute)
            .sink { action in
                environment.mediaService.muteLocalAudio(isMuted: action.payload)
                    .sink { completion in
                        switch completion {
                            case let .failure(error):
                            environment.store?.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: error.localizedMessage)))
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
    
    let updateLocalVideoView = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: MediaActions.updateLocalVideoView)
            .sink { action in
                environment.mediaService.setLocalVideoView(view: action.payload)
            }
    }
    
    let switchMirror = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.switchMirror)
            .flatMap{ action in
                environment.mediaService.switchMirror(enable: action.payload)
                    .map { enable in
                        return MediaActions.updateMirror(payload: enable)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
                
            }
            .eraseToAnyPublisher()
    }
    
    let updateVideoQuality = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.updateVideoQuality)
            .flatMap { action in
                environment.mediaService.updateVideoQuality(quality: action.payload)
                    .map {
                        return MediaActions.videoQualityUpdated(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let updateAudioQuality = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: MediaActions.updateAudioQuality)
            .flatMap { action in
                environment.mediaService.updateAudioQuality(quality: action.payload)
                    .map {
                        return MediaActions.audioQualityUpdated(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let muteAllRemoteAudio = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: MediaActions.muteAllRemoteAudio)
            .sink { action in
                environment.mediaService.muteAllRemoteAudio(isMute: action.payload)
            }
    }
    
    let stopLocalPreview = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: MediaActions.stopLocalPreview)
            .sink { action in
                environment.mediaService.stopLocalPreview()
            }
    }
}
