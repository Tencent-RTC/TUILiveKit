//
//  GlobalActions.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/5.
//

enum MediaActions {
    static let key = "Media.action"
    static let operateMicrophone = ActionTemplate(id:key.appending(".operateMicrophone"), payloadType: Bool.self)
    static let microphoneOpened = ActionTemplate(id: key.appending(".Microphone.Opened"))
    static let microphoneClosed = ActionTemplate(id: key.appending(".Microphone.Closed"))
    
    static let operateLocalAudioMute = ActionTemplate(id:key.appending(".muteLocalAudio"), payloadType: Bool.self)
    static let localAudioMuted = ActionTemplate(id:key.appending(".localAudioMuted"))
    static let localAudioUnmuted = ActionTemplate(id:key.appending(".localAudioUnmuted"))
    
    static let operateVoiceEarMonitor = ActionTemplate(id: key.appending(".EarMonitor.operated"), payloadType: Bool.self)
}

