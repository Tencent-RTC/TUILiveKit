//
//  LSUserState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine
import LiveStreamCore

struct LSUserState {
    var selfInfo: LSUser = LSUser()
    var userList: Set<LSUser> = []
    var hasAudioStreamUserList: Set<String> = []
    var hasVideoStreamUserList: Set<String> = []
    var speakingUserList: Set<String> = []
    var myFollowingUserList: Set<LSUser> = []
    var enterUserInfo: LSUser?
}

struct LSUser: Codable {
    
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
    
    init(coHostUser: CoHostUser) {
        self.userId = coHostUser.connectionUser.userId
        self.name = coHostUser.connectionUser.userName
        self.avatarUrl = coHostUser.connectionUser.avatarUrl
    }
    
    init(userId: String) {
        self.userId = userId
    }
    
    init(seatInfo: LSSeatInfo) {
        self.userId = seatInfo.userId
        self.name = seatInfo.userName
        self.avatarUrl = seatInfo.avatarUrl
    }
    
    init() {}
}

extension LSUser: Identifiable {
    var id: String {
        return self.userId
    }
}

extension LSUser: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
        hasher.combine(name)
        hasher.combine(avatarUrl)
        hasher.combine(role)
    }
    
    static func == (lhs: LSUser, rhs: LSUser) -> Bool {
        return  lhs.userId == rhs.userId && lhs.name == rhs.name && lhs.avatarUrl == rhs.avatarUrl
    }
}
