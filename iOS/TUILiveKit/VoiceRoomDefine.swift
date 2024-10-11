//
//  VoiceRoomDefine.swift
//  TUILiveKit
//
//  Created by jack on 2024/10/8.
//

import Foundation
import RTCRoomEngine

@objcMembers
public class VoiceRoomDefine: NSObject {
    
    public static let MAX_CONNECTED_VIEWERS_COUNT: Int = 10
    
}

@objcMembers
public class CreateRoomParams: NSObject {
    public var roomName: String
    public var maxAnchorCount: Int
    public var seatMode: TUISeatMode
    
    public override init() {
        self.roomName = ""
        self.maxAnchorCount = VoiceRoomDefine.MAX_CONNECTED_VIEWERS_COUNT
        self.seatMode = .applyToTake
    }
}
