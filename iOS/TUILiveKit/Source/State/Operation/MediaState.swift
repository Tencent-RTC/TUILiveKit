//
//  MediaState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct MediaState: Encodable {
    
    var hasMicrophonePermission: Bool = false
    var isMicrophoneOpened: Bool = false
    var isMicrophoneMuted: Bool = true
    
    
    var hasCameraPermission: Bool = false
    var isCameraOpened: Bool = false
    var isMirror: Bool = true
    var isFrontCamera: Bool = true
    
    var audioQuality: TUIAudioQuality = .default
    var videoQuality: TUIVideoQuality = .quality1080P
}

extension TUIAudioQuality: Codable {
    public init(from decoder: Decoder) throws {
        let value = try decoder.singleValueContainer().decode(Int.self)
        switch value {
        case 0:
            self = .speech
        case 1:
            self = .default
        case 2:
            self = .music
        default:
            self = .default
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch self {
        case .speech:
            try container.encode(0)
        case .default:
            try container.encode(1)
        case .music:
            try container.encode(2)
        default:
            try container.encode(1)
        }
    }
}

extension TUIVideoQuality: Codable {
    public init(from decoder: Decoder) throws {
        let value = try decoder.singleValueContainer().decode(Int.self)
        switch value {
        case 1:
            self = .quality360P
        case 2:
            self = .quality540P
        case 3:
            self = .quality720P
        case 4:
            self = .quality1080P
        default:
            self = .quality1080P
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch self {
        case .quality360P:
            try container.encode(1)
        case .quality540P:
            try container.encode(2)
        case .quality720P:
            try container.encode(3)
        case .quality1080P:
            try container.encode(4)
        default:
            try container.encode(4)
        }
    }
}
