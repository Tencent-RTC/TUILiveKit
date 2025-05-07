//
//  NextActionParamTuple.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//
import RTCCommon

public enum LiveStreamPrivacyStatus: NSInteger, CaseIterable {
    case `public` = 0
    case privacy = 1
    
    func getString() -> String {
        switch self {
        case .public:
            return .localized("Public")
        case .privacy:
            return .localized("Privacy")
        }
    }
}

extension LiveStreamPrivacyStatus: Codable {}

public enum LiveStreamCategory: NSInteger, CaseIterable {
    case chat = 0
    case beauty = 1
    case teach = 2
    case shopping = 3
    case music = 4
    
    func getString() -> String {
        switch self {
        case .chat:
            return .localized("Daily chat")
        case .beauty:
            return .localized("Beauty")
        case .teach:
            return .localized("Knowledge Teaching")
        case .shopping:
            return .localized("Shopping")
        case .music:
            return .localized("Music")
        }
    }
}

extension LiveStreamCategory: Codable {
    
}

public enum LinkStatus: NSInteger, Codable {
    case `none` = 0
    case applying = 1
    case linking = 2
    case pking = 3
}

public enum LiveStatus: NSInteger, Codable {
    case `none` = 0
    case previewing = 1
    case pushing = 2
    case playing = 3
    case finished = 4
}
