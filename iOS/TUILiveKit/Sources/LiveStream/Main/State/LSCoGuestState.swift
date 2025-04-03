//
//  LSCoGuestState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/3/7.
//

import RTCRoomEngine

struct LSCoGuestState {
    var lockAudioUserList: Set<String> = []
    var lockVideoUserList: Set<String> = []
    var coGuestStatus: CoGuestStatus = .none
    
    enum CoGuestStatus {
        case none
        case applying
        case linking
    }
}
