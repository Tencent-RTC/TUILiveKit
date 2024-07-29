//
//  AudioEffectService.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//

import RTCRoomEngine

import Combine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class AudioEffectEffects: Effects {
    typealias Environment = AudioEffectService
    static var id: String { "AudioEffectEffects" }
    
    let updateMusicVolume =  Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.updateMusicVolume)
            .sink { action in
                environment.updateMusicVolume(action.payload)
            }
    }
    
    let updateMicrophoneVolume =  Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.updateMicrophoneVolume)
            .sink { action in
                environment.updateMicrophoneVolume(action.payload)
            }
    }
    
    let updateMusicPitch =  Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.updateMusicPitchLevel)
            .sink { action in
                environment.updateMusicPitch(action.payload)
            }
    }
    
    let enableVoiceEarMonitor = Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.operateEarMonitor)
            .sink { action in
                environment.enableVoiceEarMonitor(enable: action.payload)
            }
    }
    
    let updateEarMonitorVolume = Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.updateEarMonitorVolume)
            .sink { action in
                environment.setVoiceEarMonitorVolume(action.payload)
            }
    }
    
    let updateChangerType = Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.changerVoice)
            .sink { action in
                environment.updateChangerType(action.payload)
            }
    }
    
    let updateReverbType = Effect<Environment>.nonDispatching { actions, environment in
        actions.wasCreated(from: AudioEffectActions.reverbVoice)
            .sink { action in
                environment.updateReverbType(action.payload)
            }
    }
}

class AudioEffectService {
    let trtcCloud: TRTCCloud
    
    init(trtcCloud: TRTCCloud) {
        self.trtcCloud = trtcCloud
    }
    
    private func audioEffectManager() -> TXAudioEffectManager {
        return trtcCloud.getAudioEffectManager()
    }
    
    func updateMusicVolume(_ volume: Int) {
        audioEffectManager().setAllMusicVolume(volume)
    }
    
    func updateMicrophoneVolume(_ volume: Int) {
        audioEffectManager().setVoiceVolume(volume)
    }
    
    func updateMusicPitch(_ pitch: Double) {
        audioEffectManager().setVoicePitch(pitch)
    }
    
    func enableVoiceEarMonitor(enable: Bool) {
        audioEffectManager().enableVoiceEarMonitor(enable)
    }
    
    func setVoiceEarMonitorVolume(_ volume: Int) {
        audioEffectManager().setVoiceEarMonitorVolume(volume)
    }
    
    func updateChangerType(_ type: AudioChangerType) {
        audioEffectManager().setVoiceChangerType(TXVoiceChangeType(rawValue: type.rawValue) ?? ._0)
    }
    
    func updateReverbType(_ type: AudioReverbType) {
        audioEffectManager().setVoiceReverbType(TXVoiceReverbType(rawValue: type.rawValue) ?? ._0)
    }
}
