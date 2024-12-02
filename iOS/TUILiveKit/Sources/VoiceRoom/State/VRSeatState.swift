//
//  VRSeatState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct VRSeatState: Encodable {
    var seatList: [VRSeatInfo] = []
    var seatApplicationList: [VRSeatApplication] = []
    var isApplyingToTakeSeat: Bool = false
}

struct VRSeatApplication: Codable {
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

struct VRSeatInfo: Codable {
    var index: Int
    var userId: String
    var avatarUrl: String
    var userName: String
    var isLocked: Bool
    var isVideoLocked: Bool
    var isAudioLocked: Bool
    
    init(info: TUISeatInfo) {
        self.index = info.index
        self.userId = info.userId ?? ""
        self.isLocked = info.isLocked
        self.isAudioLocked = info.isAudioLocked
        self.isVideoLocked = info.isVideoLocked
        self.userName = info.userName ?? ""
        self.avatarUrl = info.avatarUrl ?? ""
    }
    
    init() {
        self.index = -1
        self.userId = ""
        self.isLocked = false
        self.isVideoLocked = false
        self.isAudioLocked = false
        self.userName = ""
        self.avatarUrl = ""
    }
    
    init(userInfo: VRUser) {
        self.index = -1
        self.userId = userInfo.userId
        self.userName = userInfo.name
        self.avatarUrl = userInfo.avatarUrl
        
        self.isLocked = false
        self.isVideoLocked = false
        self.isAudioLocked = false
    }
    
}

extension VRSeatInfo: Equatable {}

extension VRSeatApplication: Equatable {}

extension VRSeatInfo: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }
}

