//
//  NextActionParamTuple.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//


// MARK: - View action params define.
typealias NextActionParamTuple<T> = (param: T, nextActions: [Action])
typealias NextActionTemplateParamTuple<T, Payload> = (param: T, nextActionTemplates: [ActionTemplate<Payload>])

public enum LiveStreamPrivacyStatus: NSInteger, CaseIterable {
    case `public` = 0
    case privacy = 1
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
            return .localized("live.category.chat")
        case .beauty:
            return .localized("live.category.beauty")
        case .teach:
            return .localized("live.category.teach")
        case .shopping:
            return .localized("live.category.shopping")
        case .music:
            return .localized("live.category.music")
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

// MARK: -View action params generator.
func generateActionParamTuple<T>(param: T, actions:[Action]) -> NextActionParamTuple<T> {
    return (param: param, nextActions: actions)
}

func generateActionTemplateParamTuple<T, Payload>(param: T, actions:[ActionTemplate<Payload>]) -> NextActionTemplateParamTuple<T, Payload> {
    return (param: param, nextActionTemplates: actions)
}
