//
//  CoHostState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

struct CoHostState {
    var connectedUserList: [TUIConnectionUser] = []
    var sentConnectionRequestList: [TUIConnectionUser] = []
    var receivedConnectionRequest: TUIConnectionUser? = nil
    
    var enableConnection: Bool = true
}
