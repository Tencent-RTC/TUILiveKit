//
//  User.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

public class TUIBarrageUser: Codable {
    public var userId: String
    public var userName: String
    public var avatarUrl: String
    public var level: String
    public init(userId: String = "", userName: String = "", avatarUrl: String = "", level: String = "") {
        self.userId = userId
        self.userName = userName
        self.avatarUrl = avatarUrl
        self.level = level
    }
    
    public enum CodingKeys: String, CodingKey {
        case userId
        case userName
        case avatarUrl
        case level
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(userId, forKey: .userId)
        try container.encode(userName, forKey: .userName)
        try container.encode(avatarUrl, forKey: .avatarUrl)
        try container.encode(level, forKey: .level)
    }
    
    public required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        userId = try container.decode(String.self, forKey: .userId)
        userName = try container.decode(String.self, forKey: .userName)
        avatarUrl = try container.decode(String.self, forKey: .avatarUrl)
        level = try container.decode(String.self, forKey: .level)
    }
}
