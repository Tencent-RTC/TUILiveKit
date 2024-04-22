//
//  User.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

class TUIBarrageUser: Codable {
    var userId: String
    var userName: String
    var avatarUrl: String
    var level: String
    init(userId: String = "", userName: String = "", avatarUrl: String = "", level: String = "") {
        self.userId = userId
        self.userName = userName
        self.avatarUrl = avatarUrl
        self.level = level
    }
    
    enum CodingKeys: String, CodingKey {
        case userId
        case userName
        case avatarUrl
        case level
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(userId, forKey: .userId)
        try container.encode(userName, forKey: .userName)
        try container.encode(avatarUrl, forKey: .avatarUrl)
        try container.encode(level, forKey: .level)
    }
    
    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        userId = try container.decode(String.self, forKey: .userId)
        userName = try container.decode(String.self, forKey: .userName)
        avatarUrl = try container.decode(String.self, forKey: .avatarUrl)
        level = try container.decode(String.self, forKey: .level)
    }
}
