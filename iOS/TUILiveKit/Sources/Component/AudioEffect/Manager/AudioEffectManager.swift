//
//  AudioEffectManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/9.
//

import Foundation
import RTCCommon
import Combine
import AtomicXCore

class AudioEffectManager {
    private lazy var provider = AudioEffectDataProvider(manager: self)
    
    private var audioEffectStore: AudioEffectStore {
        return AudioEffectStore.shared
    }
    
    private var deviceStore: DeviceStore {
        return DeviceStore.shared
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
    func setMicrophoneVolume(_ volume: Int) {
        deviceStore.setOutputVolume(volume)
    }
    
    func setVoiceEarMonitorEnable(_ enable: Bool) {
        audioEffectStore.setVoiceEarMonitorEnable(enable: enable)
    }
    
    func setVoiceEarMonitorVolume(_ volume: Int) {
        audioEffectStore.setVoiceEarMonitorVolume(volume: volume)
    }
    
    func setChangerType(_ type: AudioChangerType) {
        audioEffectStore.setAudioChangerType(type: type)
    }
    
    func setReverbType(_ type: AudioReverbType) {
        audioEffectStore.setAudioReverbType(type: type)
    }
}

// MARK: - Tools
extension AudioEffectManager {
    var audioState: AudioEffectState {
        audioEffectStore.state.value
    }
    
    var deviceState: DeviceState {
        deviceStore.state.value
    }
    
    func subscribeState<Value>(_ selector: StatePublisherSelector<AudioEffectState, Value>) -> AnyPublisher<Value, Never> {
        return audioEffectStore.state.subscribe(selector)
    }
    
    func subscribeState<Value>(_ selector: StatePublisherSelector<DeviceState, Value>) -> AnyPublisher<Value, Never> {
        return deviceStore.state.subscribe(selector)
    }
}

