//
//  VRMediaState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct VRMediaState: Encodable {
    var hasMicrophonePermission: Bool = false
    var isMicrophoneOpened: Bool = false
    var isMicrophoneMuted: Bool = true
    var audioQuality: TUIAudioQuality = .default
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
