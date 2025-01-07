//
//  LSCoGuestState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import RTCRoomEngine

struct LSCoGuestState {
    var connectedUserList: [LSSeatInfo] = []
    var requestCoGuestList: Set<LSSeatApplication> = []
    var coGuestStatus: CoGuestStatus = .none
}

struct LSSeatApplication: Codable {
    var id: String
    var userId: String
    var avatarUrl: String
    var userName: String
    var content: String
    var timestamp: UInt
    
    init(request: TUIRequest) {
        self.id = request.requestId
        self.userId = request.userId
        self.avatarUrl = request.avatarUrl
        self.userName = request.userName
        self.content = request.content
        self.timestamp = request.timestamp
    }
    
    init(userInfo: TUIUserInfo) {
        self.id = ""
        self.userId = userInfo.userId
        self.avatarUrl = userInfo.avatarUrl
        self.userName = userInfo.userName
        self.content = ""
        self.timestamp = 0
    }
}

extension LSSeatApplication: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }
}

struct LSSeatInvitation: Codable {
    var id: String
    var userId: String
    var avatarUrl: String
    var userName: String
    var content: String
    var timestamp: UInt
    
    init(request: TUIRequest) {
        self.id = request.requestId
        self.userId = request.userId
        self.avatarUrl = request.avatarUrl
        self.userName = request.userName
        self.content = request.content
        self.timestamp = request.timestamp
    }
    
    init() {
        self.id = ""
        self.userId = ""
        self.avatarUrl = ""
        self.userName = ""
        self.content = ""
        self.timestamp = 0
    }
}

struct LSSeatInfo: Codable {
    var index: Int = -1
    var userId: String = ""
    var avatarUrl: String = ""
    var userName: String = ""
    var isLocked: Bool = false
    var isVideoLocked: Bool = false
    var isAudioLocked: Bool = false
    
    init(info: TUISeatInfo) {
        self.index = info.index
        self.userId = info.userId ?? ""
        self.isLocked = info.isLocked
        self.isAudioLocked = info.isAudioLocked
        self.isVideoLocked = info.isVideoLocked
        self.userName = info.userName ?? ""
        self.avatarUrl = info.avatarUrl ?? ""
    }
    
    init(userInfo: TUIUserInfo) {
        self.index = -1
        self.userId = userInfo.userId
        self.userName = userInfo.userName
        self.avatarUrl = userInfo.avatarUrl
        
        self.isLocked = false
        self.isVideoLocked = false
        self.isAudioLocked = false
    }
    
}

extension LSSeatInfo: Equatable {}

extension LSSeatApplication: Equatable {}

extension LSSeatInvitation: Equatable {}

extension LSSeatInfo: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }

}

enum CoGuestStatus {
    case none
    case applying
    case linking
}
