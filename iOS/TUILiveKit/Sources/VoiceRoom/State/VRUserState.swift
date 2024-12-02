//
//  VRUserState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine
import ImSDK_Plus

struct VRUserState: Encodable {
    var selfInfo: VRUser = VRUser()
    var userList: [VRUser] = []
    var myFollowingUserList: Set<VRUser> = []
    var hasAudioStreamUserList: Set<String> = []
    var speakingUserList: Set<String> = []
}

struct VRUser: Codable {
    
    var userId: String = ""
    var name: String = ""
    var avatarUrl: String = ""
    var role: TUIRole = .generalUser
    var fansCount: Int = 0
    var linkStatus: LinkStatus = .none
    
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
    
    init(userInfo: V2TIMUserFullInfo) {
        self.userId = userInfo.userID
        self.name = userInfo.nickName
        self.avatarUrl = userInfo.faceURL
    }
    
    init(userId: String) {
        self.userId = userId
    }
    
    init(seatInfo: VRSeatInfo) {
        self.userId = seatInfo.userId
        self.name = seatInfo.userName
        self.avatarUrl = seatInfo.avatarUrl
    }
    
    init() {}
}

extension VRUser: Identifiable {
    var id: String {
        return self.userId
    }
}

extension VRUser: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
        hasher.combine(name)
        hasher.combine(avatarUrl)
        hasher.combine(role)
    }
    
    static func == (lhs: VRUser, rhs: VRUser) -> Bool {
        return  lhs.userId == rhs.userId && lhs.name == rhs.name && lhs.avatarUrl == rhs.avatarUrl
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
