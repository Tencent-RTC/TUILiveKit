//
// TUIBarrage.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import RTCCommon

public class TUIBarrage: Codable{
    public var user: TUIBarrageUser
    public var content: String
    public var extInfo: [String: AnyCodable]

    public init(user: TUIBarrageUser = TUIBarrageUser(), content: String = "", extInfo: [String: AnyCodable] = [:]) {
        self.user = user
        self.content = content
        self.extInfo = extInfo
    }

    public enum CodingKeys: String, CodingKey {
        case user
        case content
        case extInfo
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(user, forKey: .user)
        try container.encode(content, forKey: .content)
        try container.encode(extInfo, forKey: .extInfo)
    }
    
    public required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        user = try container.decode(TUIBarrageUser.self, forKey: .user)
        content = try container.decode(String.self, forKey: .content)
        extInfo = try container.decode([String: AnyCodable].self, forKey: .extInfo)
    }
    
    public func clone() -> TUIBarrage {
        return TUIBarrage(user: user, content: content, extInfo: extInfo)
    }
}
