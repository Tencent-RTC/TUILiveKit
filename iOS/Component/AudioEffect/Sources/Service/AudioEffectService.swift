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
import TUILiveResources

class AudioEffectService: AudioEffectManagerInterface {
    func setMicrophoneVolume(_ volume: Int) {
        audioEffectManager.setVoiceVolume(volume)
    }
    
    func setMusicPitch(_ pitch: Double) {
        audioEffectManager.setVoicePitch(pitch)
    }
    
    func setVoiceEarMonitorEnable(_ enable: Bool) {
        audioEffectManager.enableVoiceEarMonitor(enable)
    }
    
    func setVoiceEarMonitorVolume(_ volume: Int) {
        audioEffectManager.setVoiceEarMonitorVolume(volume)
    }
    
    func setChangerType(_ type: AudioChangerType) {
        audioEffectManager.setVoiceChangerType(TXVoiceChangeType(rawValue: type.rawValue) ?? ._0)
    }
    
    func setReverbType(_ type: AudioReverbType) {
        audioEffectManager.setVoiceReverbType(TXVoiceReverbType(rawValue: type.rawValue) ?? ._0)
    }
}

extension AudioEffectService {
    private var audioEffectManager: TXAudioEffectManager {
        TUIRoomEngine.sharedInstance().getTRTCCloud().getAudioEffectManager()
    }
    
    static func resetAudioSettings() {
        LiveKitLog.info("\(#file)", "\(#line)", "AudioEffectView reset effects")
        
        let service = AudioEffectService()
        service.setVoiceEarMonitorEnable(false)
        service.setVoiceEarMonitorVolume(100)
        
        service.setMicrophoneVolume(100)
        
        service.setChangerType(.none)
        service.setReverbType(.none)
    }
}
