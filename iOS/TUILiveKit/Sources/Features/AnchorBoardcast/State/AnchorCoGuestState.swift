//
//  AnchorCoGuestState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/3/7.
//

import RTCRoomEngine

struct AnchorCoGuestState {
    var lockAudioUserList: Set<String> = []
    var lockVideoUserList: Set<String> = []
    var coGuestStatus: AnchorCoGuestStatus = .none
    
    enum AnchorCoGuestStatus {
        case none
        case applying
        case linking
    }
}
