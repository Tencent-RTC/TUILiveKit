//
//  VoiceRoomNavigationState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import Foundation

struct ViewState {
    var linkStatus: LinkStatus = .none
    var liveStatus: LiveStatus = .previewing
    var autoOpenCameraOnSeated: Bool = true
}
