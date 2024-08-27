//
//  OpreationState.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/16.
//

import Foundation

// All room effect state define.
struct OperationState: Encodable {
    var mediaState = MediaState()
    var roomState = RoomState()
    var seatState = SeatState()
    var userState = UserState()
    var beautyState = BeautyState()
    var connectionState = ConnectionState()
}
