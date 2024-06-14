//
//  RoomState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

// PassthroughSubject
// CurrentValueSubject

import Foundation
import RTCRoomEngine

struct RoomState: Codable {
    var roomId: String = ""
    var createTime: UInt = 0
    var ownerInfo: User = User()
    var roomName: String = ""
    var coverURL: String = String.randomBackgroundImageUrl()
    
    
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

extension RoomState: Identifiable {
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

fileprivate extension String {
    static func randomBackgroundImageUrl() -> String {
        let random = arc4random() % 12 + 1
        return "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover\(random).png"
    }
}
