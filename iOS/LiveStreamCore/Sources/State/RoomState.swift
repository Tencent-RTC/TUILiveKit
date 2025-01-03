//
//  RoomState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct RoomState {
    public var roomId: String = ""
    public var ownerInfo: TUIUserInfo = TUIUserInfo()
    var maxCoGuestCount: Int = 0
    var liveStatus: LiveStatus = .none

    enum LiveStatus {
        case none
        case pushing
        case playing
    }
}
