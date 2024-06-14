//
//  GlobalActions.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/5.
//

import RTCRoomEngine

enum CameraDirection {
    case front, rear
}

enum MediaActions {
    static let key = "Media.action"
    
    static let operateMicrophone = ActionTemplate(id:key.appending(".OperateMicrophone"), payloadType: Bool.self)
    static let microphoneOpened = ActionTemplate(id: key.appending(".Microphone.Opened"))
    static let microphoneClosed = ActionTemplate(id: key.appending(".Microphone.Closed"))
    
    static let operateCamera = ActionTemplate(id:key.appending(".operateCamera"), payloadType: Bool.self)
    static let cameraOpened = ActionTemplate(id: key.appending(".Camera.Opened"), payloadType: CameraDirection.self)
    static let cameraClosed = ActionTemplate(id: key.appending(".Camera.Closed"))
    
    static let switchCamera = ActionTemplate(id:key.appending(".SwitchCamera"), payloadType: CameraDirection.self)
    static let frontCameraOpened = ActionTemplate(id: key.appending(".Front.Camera.Opened"))
    static let rearCameraOpened = ActionTemplate(id: key.appending(".Rear.Camera.Opened"))
    
    static let operateMicrophoneMute = ActionTemplate(id:key.appending(".operateMicrophoneMute"), payloadType: Bool.self)
    static let microphoneMuted = ActionTemplate(id:key.appending(".Microphone.Muted"))
    static let microphoneUnmuted = ActionTemplate(id:key.appending(".Microphone.Unmuted"))
    static let updateLocalVideoView = ActionTemplate(id: key.appending(".updateLocalVideoView"), payloadType: UIView.self)
    static let switchMirror = ActionTemplate(id:key.appending(".Switch.Mirror"), payloadType: Bool.self)
    static let updateMirror = ActionTemplate(id: key.appending(".Update.Mirror"), payloadType: Bool.self)
    static let updateVideoQuality = ActionTemplate(id: key.appending(".UpdateVideoQuality"), payloadType: TUIVideoQuality.self)
    static let updateAudioQuality = ActionTemplate(id: key.appending(".UpdateAudioQuality"), payloadType: TUIAudioQuality.self)
    static let videoQualityUpdated = ActionTemplate(id: key.appending(".VideoQualityUpdated"), payloadType: TUIVideoQuality.self)
    static let audioQualityUpdated = ActionTemplate(id: key.appending(".AudioQualityUpdated"), payloadType: TUIAudioQuality.self)
    static let muteAllRemoteAudio = ActionTemplate(id: key.appending(".MuteAllRemoteAudio"), payloadType: Bool.self)
    static let stopLocalPreview = ActionTemplate(id: key.appending(".stopLocalPreview"))
    
}

