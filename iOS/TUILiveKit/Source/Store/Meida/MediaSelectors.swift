//
//  MediaSelectors.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

enum MediaSelectors {
    static let getMediaState = Selector(keyPath: \OperationState.mediaState)
    static let getMicrophoneState = Selector.with(getMediaState, keyPath: \.isMicrophoneOpened)
    static let getFrontCameraState = Selector.with(getMediaState, keyPath: \.isFrontCamera)
    static let getMirrorState = Selector.with(getMediaState, keyPath: \.isMirror)
    static let getAudioQuality = Selector.with(getMediaState, keyPath: \.audioQuality)
    static let getVideoQuality = Selector.with(getMediaState, keyPath: \.videoQuality)
}
