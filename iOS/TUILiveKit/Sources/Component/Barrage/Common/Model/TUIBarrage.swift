//
// TUIBarrage.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//


class TUIBarrage: Codable{
    var user: TUIBarrageUser
    var content: String
    var extInfo: [String: AnyCodable]

    init(user: TUIBarrageUser = TUIBarrageUser(), content: String = "", extInfo: [String: AnyCodable] = [:]) {
        self.user = user
        self.content = content
        self.extInfo = extInfo
    }

    enum CodingKeys: String, CodingKey {
        case user
        case content
        case extInfo
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(user, forKey: .user)
        try container.encode(content, forKey: .content)
        try container.encode(extInfo, forKey: .extInfo)
    }
    
    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        user = try container.decode(TUIBarrageUser.self, forKey: .user)
        content = try container.decode(String.self, forKey: .content)
        extInfo = try container.decode([String: AnyCodable].self, forKey: .extInfo)
    }
    
    func clone() -> TUIBarrage {
        return TUIBarrage(user: user, content: content, extInfo: extInfo)
    }
}
