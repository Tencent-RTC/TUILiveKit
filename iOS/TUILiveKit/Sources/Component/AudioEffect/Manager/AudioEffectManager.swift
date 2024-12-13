//
//  AudioEffectManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/9.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine

protocol AudioEffectManagerInterface {
    func setMusicVolume(_ volume: Int)
    func setMicrophoneVolume(_ volume: Int)
    func setVoiceEarMonitorVolume(_ volume: Int)
    
    func setVoiceEarMonitorEnable(_ enable: Bool)
    
    func setMusicPitch(_ pitch: Double)
    func setChangerType(_ type: AudioChangerType)
    func setReverbType(_ type: AudioReverbType)
}

class AudioEffectManager: AudioEffectManagerInterface {
    private let stateKey = "__kAudioEffectManager_state_key__"
    
    private let service = AudioEffectService()
    private lazy var provider = AudioEffectDataProvider(manager: self)
    
    init() {
        StateCache.shared.subscribeToObjectRemoval(key: stateKey) {
            AudioEffectService.resetAudioSettings()
        }
    }
}

// MARK: - AudioEffectMenuDateGenerator
extension AudioEffectManager: AudioEffectMenuDateGenerator {
    var audioEffectMenus: [Section : [SettingItem]] {
        provider.audioEffectMenus
    }
    
    var audioEffectSectionTitles: [Section : String] {
        provider.audioEffectSectionTitles
    }
}

// MARK: - AudioEffectManagerInterface
extension AudioEffectManager {
    func setMusicVolume(_ volume: Int) {
        service.setMusicVolume(volume)
        update { state in
            state.musicVolume = volume
        }
    }
    
    func setMicrophoneVolume(_ volume: Int) {
        service.setMicrophoneVolume(volume)
        update { state in
            state.microphoneVolume = volume
        }
    }
    
    func setMusicPitch(_ pitch: Double) {
        service.setMusicPitch(pitch)
        update { state in
            state.voicePitch = pitch
        }
    }
    
    func setVoiceEarMonitorEnable(_ enable: Bool) {
        service.setVoiceEarMonitorEnable(enable)
        update { state in
            state.isEarMonitorOpened = enable
        }
    }
    
    func setVoiceEarMonitorVolume(_ volume: Int) {
        service.setVoiceEarMonitorVolume(volume)
        update { state in
            state.earMonitorVolume = volume
        }
    }
    
    func setChangerType(_ type: AudioChangerType) {
        service.setChangerType(type)
        update { state in
            state.changerType = type
        }
    }
    
    func setReverbType(_ type: AudioReverbType) {
        service.setReverbType(type)
        update { state in
            state.reverbType = type
        }
    }
}

// MARK: - Tools
extension AudioEffectManager {
    var state: AudioEffectState {
        observerState.state
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AudioEffectState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeState() -> AnyPublisher<AudioEffectState, Never> {
        return observerState.subscribe()
    }
}

// MARK: - Private functions
extension AudioEffectManager {
    private typealias AudioEffectStateUpdateClosure = (inout AudioEffectState) -> Void
    private var observerState: ObservableState<AudioEffectState> {
        if let state: ObservableState<AudioEffectState> = StateCache.shared[stateKey] {
            return state
        }
        let newState = ObservableState<AudioEffectState>(initialState: AudioEffectState())
        StateCache.shared.setObject(key: stateKey, obj: newState)
        if let state: ObservableState<AudioEffectState> = StateCache.shared[stateKey] {
            return state
        }
        return newState
    }
    private func update(state: AudioEffectStateUpdateClosure) {
        observerState.update(reduce: state)
    }
}
