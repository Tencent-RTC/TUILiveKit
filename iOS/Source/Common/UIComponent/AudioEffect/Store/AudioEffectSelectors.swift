//
//  AudioEffectSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//

enum AudioEffectSelectors {
    static let isEarMonitorOpened = Selector(keyPath: \AudioEffectState.isEarMonitorOpened)
    static let earMonitorVolume = Selector(keyPath: \AudioEffectState.earMonitorVolume)
    static let musicVolume = Selector(keyPath: \AudioEffectState.musicVolume)
    static let microphoneVolume = Selector(keyPath: \AudioEffectState.microphoneVolume)
    static let voicePitch = Selector(keyPath: \AudioEffectState.voicePitch)
    static let currentPlayMusic = Selector(keyPath: \AudioEffectState.currentPlayMusic)
    static let changerType = Selector(keyPath: \AudioEffectState.changerType)
    static let reverbType = Selector(keyPath: \AudioEffectState.reverbType)
    
}

