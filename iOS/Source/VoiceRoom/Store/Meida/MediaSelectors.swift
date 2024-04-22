//
//  MediaSelectors.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

enum MediaSelectors {
    static let getMediaState = Selector(keyPath: \OperationState.mediaState)
    static let getMicrophoneState = Selector.with(getMediaState, keyPath: \.isMicrophoneOpened)
}
