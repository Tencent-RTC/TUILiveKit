//
//  LSMediaState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine

struct LSMediaState {
    var isMicrophoneOpened: Bool = false
    var isMicrophoneMuted: Bool = true
    var audioQuality: TUIAudioQuality = .default
    var isCameraOpened: Bool = false
    var videoQuality: TUIVideoQuality = .quality1080P
    var isMirror: Bool = true
    var isFrontCamera: Bool = true
}
