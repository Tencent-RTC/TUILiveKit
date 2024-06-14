//
//  AudioEffectAction.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//


enum AudioEffectActions {
    static let key = "AudioEffect.action"
    static let operateEarMonitor = ActionTemplate(id: key.appending(".operateEarMonitor"), payloadType: Bool.self)
    static let updateEarMonitorVolume = ActionTemplate(id: key.appending(".updateEarMonitorVolume"), payloadType: Int.self)
    static let updateMusicVolume = ActionTemplate(id: key.appending(".operateEarMonitor"), payloadType: Int.self)
    static let updateMicrophoneVolume = ActionTemplate(id: key.appending(".updateMicrophoneVolume"), payloadType: Int.self)
    static let updateMusicPitchLevel = ActionTemplate(id: key.appending(".updateMusicPitchLevel"), payloadType: Double.self)
    
    static let selectMusic = ActionTemplate(id: key.appending(".operateEarMonitor"), payloadType: String.self)
    
    static let changerVoice = ActionTemplate(id: key.appending(".operateEarMonitor"), payloadType: AudioChangerType.self)
    static let reverbVoice = ActionTemplate(id: key.appending(".operateEarMonitor"), payloadType: AudioReverbType.self)
}

