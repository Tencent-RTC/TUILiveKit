//
//  UserState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct UserState: Encodable {
    var currentUser: User = User()
    var roomOwner: User?
    var audienceList: [User] = []
    var speakingUsers: Set<String> = []
    var audioAvailableUsers: Set<String> = []
    var receivedGiftTotalPrice: Int = 0
    var sendGiftUsers: Set<String> = []
}

struct User: Codable {
    
    var userId: String = ""
    var name: String = ""
    var avatarUrl: String = ""
    var role: TUIRole = .generalUser
    
    init(userInfo: TUIUserInfo) {
        self.userId = userInfo.userId
        self.name = userInfo.userName
        self.avatarUrl = userInfo.avatarUrl
        self.role = userInfo.userRole
    }
    
    init(loginInfo: TUILoginUserInfo) {
        self.userId = loginInfo.userId
        self.name = loginInfo.userName
        self.avatarUrl = loginInfo.avatarUrl
    }
    
    init() {}
}

extension User: Identifiable {
    var id: String {
        return self.userId
    }
}

extension User: Equatable {
    static func == (lhs: User, rhs: User) -> Bool {
        return lhs.id == rhs.id
    }
}

extension TUIRole: Codable {
    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let rawValue = try container.decode(UInt.self)
        self = TUIRole(rawValue: rawValue) ?? .generalUser
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}


