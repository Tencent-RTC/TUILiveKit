//
//  SeatGridConfiguration.swift
//  SeatGridView
//
//  Created by AI Assistant on 2024/10/16.
//

import SwiftUI

public enum SeatLayoutMode: CaseIterable {
    case singleRow
    case multiRow
}

public enum SeatStatusType: String, CaseIterable {
    case speaking = "speaking"
    case preparing = "preparing"
    case listening = "listening"
    case away = "away"
    
    public var color: Color {
        switch self {
        case .speaking: return .red
        case .preparing: return .orange
        case .listening: return .green
        case .away: return .gray
        }
    }
    
    public var iconName: String {
        switch self {
        case .speaking: return "mic.fill"
        case .preparing: return "clock.fill"
        case .listening: return "ear.fill"
        case .away: return "person.slash.fill"
        }
    }
}

public struct SeatGridConfiguration {
    public var layoutMode: SeatLayoutMode = .singleRow
    
    public var seatSize: CGSize = CGSize(width: 65, height: 80)
    
    public var spacing: CGFloat = 20
    
    public var showStatusBadge: Bool = false
    
    public var enableUserInteraction: Bool = true
    
    public var ownerSeatFixed: Bool = true
    
    public var showDivider: Bool = true

    public var statusBadgeConfig: StatusBadgeConfiguration = StatusBadgeConfiguration()
    
    public init() {}
    
    public init(layoutMode: SeatLayoutMode) {
        self.layoutMode = layoutMode
    }
}

public struct StatusBadgeConfiguration {
    public var fontSize: CGFloat = 10
    
    public var cornerRadius: CGFloat = 8
    
    public var padding: EdgeInsets = EdgeInsets(top: 2, leading: 6, bottom: 2, trailing: 6)
    
    public var opacity: Double = 0.9
    
    public init() {}
}


public extension SeatGridConfiguration {
    static func singleRowConfiguration() -> SeatGridConfiguration {
        var config = SeatGridConfiguration()
        config.layoutMode = .singleRow
        config.ownerSeatFixed = true
        config.showDivider = true
        return config
    }
    
    static func multiRowConfiguration() -> SeatGridConfiguration {
        var config = SeatGridConfiguration()
        config.spacing = 8
        config.layoutMode = .multiRow
        config.ownerSeatFixed = false
        config.showDivider = false
        config.seatSize = CGSize(width: 65, height: 85)
        return config
    }

    
    static func customConfiguration(
        layoutMode: SeatLayoutMode,
        seatSize: CGSize,
        spacing: CGFloat
    ) -> SeatGridConfiguration {
        var config = SeatGridConfiguration()
        config.layoutMode = layoutMode
        config.seatSize = seatSize
        config.spacing = spacing
        return config
    }
} 
