//
//  LiveIdentityGenerator.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/6/6.
//

import Foundation

@objcMembers
public final class LiveIdentityGenerator: NSObject {
    @objc public enum RoomType: Int, CaseIterable {
        case live
        case voice
        case ktv
        case unknown
    }

    public static let shared = LiveIdentityGenerator()
    private override init() {
    }

    public func generateId(_ id: String, type: RoomType) -> String {
        return getPrefix(type: type) + id
    }

    public func getIDType(_ id: String) -> RoomType {
        for roomType in RoomType.allCases {
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
        case .ktv:
            return "ktv_"
        case .unknown:
            return ""
        }
    }
}
