//
//  LiveIdentityGenerator.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/6/6.
//

import Foundation

@objcMembers
public final class LiveIdentityGenerator: NSObject {
    @objc public enum RoomType: Int {
        case live
        case voice
        case unknown
    }

    public static let shared = LiveIdentityGenerator()
    private override init() {
    }

    public func generateId(_ id: String, type: RoomType) -> String {
        return getPrefix(type: type) + id
    }

    public func getIDType(_ id: String) -> RoomType {
        let roomTypeArray = [RoomType.live, RoomType.voice]
        for roomType in roomTypeArray {
            if id.hasPrefix(getPrefix(type: roomType)) {
                return roomType
            }
        }
        return .unknown
    }
    
    private func getPrefix(type: RoomType) -> String {
        switch type {
        case .live:
            return "live_"
        case .voice:
            return "voice_"
        case .unknown:
            return "unknown_"
        }
    }
}
