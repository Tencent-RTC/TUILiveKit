//
//  RoomState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

enum EnterRoomState {
    case notEntered
    case inRoom
}

extension EnterRoomState: Codable {
    
    init(from decoder: Decoder) throws {
        let value = try decoder.singleValueContainer().decode(String.self)
        switch value {
            case "notEntered":
                self = .notEntered
            case "inRoom":
                self = .inRoom
            default:
                self = .notEntered
        }
    }
    
    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch self {
            case.notEntered:
                try container.encode("notEntered")
            case.inRoom:
                try container.encode("inRoom")
        }
    }
}

struct RoomState: Codable {
    var roomId: String = ""
    var createTime: UInt = 0
    var ownerId: String = ""
    var name: String = ""
    var seatMode: TUISeatMode = .applyToTake
    var seatCount: Int = 8
    var memberCount: Int = 0
    // custom info.
    var coverURL: String = String.randomBackgroundImageUrl()
    var category: String = LiveStreamCategory.chat.getString()
    var mode: String = LiveMode.public.getString()
    // internal state
    var enterRoomState: EnterRoomState = .notEntered
    
}

extension RoomState: Identifiable {
    var id: String {
        self.roomId
    }
}

fileprivate extension String {
    static func randomBackgroundImageUrl() -> String {
        let random = arc4random() % 12 + 1
        return "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover\(random).png"
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
