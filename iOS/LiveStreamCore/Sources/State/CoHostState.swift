//
//  CoHostState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct CoHostState: State {
    public var connectedUserList: [TUIConnectionUser] = []
    public var sentConnectionRequestList: [TUIConnectionUser] = []
    public var receivedConnectionRequest: TUIConnectionUser? = nil
    
    public var enableConnection: Bool = true
    
    public init() {}
}
