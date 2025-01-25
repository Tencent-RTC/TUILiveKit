//
//  CoGuestState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct CoGuestState: State {
    public var coGuestStatus: CoGuestStatus = .none
    public var connectedUserList: [TUISeatInfo] = []
    public var connectionRequestList: Set<TUIRequest> = []
    public var myRequestId: String = ""
    public var openCameraOnCoGuest: Bool = true
    public var enableConnection: Bool = true
    
    public enum CoGuestStatus {
        case none
        case applying
        case linking
    }
    
    public init() {}
}
