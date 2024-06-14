//
//  LiveIdentityGenerator.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/6/6.
//

import Foundation

public final class LiveIdentityGenerator {
    public enum RoomType: CaseIterable {
        case live
        case voice
        var prefix: String {
            switch self {
            case .live:
                return "live_"
            case .voice:
                return "voice_"
            }
        }
    }

    public static let shared = LiveIdentityGenerator()
    private init() {
    }

    public func generateId(_ id: String, _ type: RoomType) -> String {
        return type.prefix + id
    }

    public func getIDType(_ id: String) -> RoomType? {
        for type in RoomType.allCases {
            if id.hasPrefix(type.prefix) {
                return type
            }
        }
        return nil
    }
}
