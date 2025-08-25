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

public enum LiveTemplateMode: NSInteger, CaseIterable {
    case verticalGridDynamic = 600
    case verticalFloatDynamic = 601
    case verticalGridStatic = 800
    case verticalFloatStatic = 801
    
    func toString() -> String {
        switch self {
        case .verticalGridDynamic:
            return internalLocalized("Dynamic grid layout")
        case .verticalFloatDynamic:
            return internalLocalized("Dynamic float layout")
        case .verticalGridStatic:
            return internalLocalized("Static grid layout")
        case .verticalFloatStatic:
            return internalLocalized("Static float layout")
        }
    }
    
    func toImageName() -> String {
        switch self {
        case .verticalGridDynamic:
            return "dynamicGridLayout"
        case .verticalFloatDynamic:
            return "dynamicFloatLayout"
        case .verticalGridStatic:
            return "staticGridLayout"
        case .verticalFloatStatic:
            return "staticFloatLayout"
        }
    }
    
    func toPkImageName() -> String {
        switch self {
        case .verticalGridDynamic:
            return "pk_dynamicGridLayout"
        case .verticalFloatDynamic:
            return "pk_dynamicFloatLayout"
        default:
            assert(false, "Not support")
            return ""
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
