//
//  MediaState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation

struct MediaState: Encodable {
    // TODO: - read this value from system permission
    // default value: read from system permission.
    var hasMicrophonePermission: Bool = true
    // default value: microphone muted state is true.
    var isMicrophoneOpened: Bool = true
    // default value: in-ear monitors is not opened.
    var isEarMonitorOpened: Bool = false
    // default value: local audio stream is muted.
    var isLocalAudioStreamMuted: Bool = true
}
