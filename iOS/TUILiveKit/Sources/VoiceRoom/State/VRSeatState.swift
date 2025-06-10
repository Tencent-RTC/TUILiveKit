//
//  VRSeatState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct VRSeatState: Encodable {
    var seatApplicationList: [VRSeatApplication] = []
    var isApplyingToTakeSeat: Bool = false
    var invitedUserIds: Set<String> = []
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

extension VRSeatApplication: Equatable {}
