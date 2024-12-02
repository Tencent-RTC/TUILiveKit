//
//  SGRoomState.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

import RTCRoomEngine

struct SGRoomState {
    var ownerId: String = ""
    var ownerName: String = ""
    var ownerAvatar: String = ""
    var seatMode: TUISeatMode = .applyToTake
    var maxSeatCount: Int = kSGDefaultMaxSeatCount
}
