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
            return internalLocalized("Public")
        case .privacy:
            return internalLocalized("Privacy")
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
            return internalLocalized("Daily chat")
        case .beauty:
            return internalLocalized("Beauty")
        case .teach:
            return internalLocalized("Knowledge Teaching")
        case .shopping:
            return internalLocalized("Shopping")
        case .music:
            return internalLocalized("Music")
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
    case pushing = 2
    case playing = 3
    case finished = 4
}
