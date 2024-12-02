//
//  CoGuestState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

struct CoGuestState {
    var coGuestStatus: CoGuestStatus = .none
    var connectedUserList: [TUISeatInfo] = []
    var connectionRequestList: Set<TUIRequest> = []
    var myRequestId: String = ""
    var openCameraOnCoGuest: Bool = true
    var enableConnection: Bool = true
    
    enum CoGuestStatus {
        case none
        case applying
        case linking
    }
}
