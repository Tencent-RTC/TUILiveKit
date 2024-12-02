//
//  VRRoomState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

// PassthroughSubject
// CurrentValueSubject

import Foundation
import RTCRoomEngine

public enum RoomPrivacyMode: NSInteger, CaseIterable {
    case `public` = 0
    case privacy = 1
}

struct VRRoomState: Codable {
    var roomId: String = ""
    var createTime: UInt = 0
    var ownerInfo: VRUser = VRUser()
    var roomName: String = ""
    var coverURL: String = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png"
    var backgroundURL: String = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png"    
    var seatMode: TUISeatMode = .applyToTake
    var userCount: Int = 0
    var maxSeatCount: Int = 0
    var liveExtraInfo: LiveExtraInfo = LiveExtraInfo()
    
    struct LiveExtraInfo: Codable {
        var category: LiveStreamCategory = .chat
        var liveMode: LiveStreamPrivacyStatus = .public
        var maxAudienceCount: Int = 0
        var messageCount: Int = 0
        var giftIncome: Int = 0
        var giftPeopleSet: Set<String> = []
        var likeCount: Int = 0
    }
}

extension VRRoomState: Identifiable {
    var id: String {
        self.roomId
    }
}

extension TUISeatMode: Codable {
    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let rawValue = try container.decode(UInt.self)
        self = TUISeatMode(rawValue: rawValue) ?? .applyToTake
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}
