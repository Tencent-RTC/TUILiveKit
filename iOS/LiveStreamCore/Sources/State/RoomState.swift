//
//  RoomState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct RoomState: State {
    public var roomId: String = ""
    public var ownerInfo: TUIUserInfo = TUIUserInfo()
    public var maxCoGuestCount: Int = 0
    public var liveStatus: LiveStatus = .none

    public enum LiveStatus {
        case none
        case pushing
        case playing
    }
    
    public init() {}
}
