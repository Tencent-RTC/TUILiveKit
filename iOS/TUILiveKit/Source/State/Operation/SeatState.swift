//
//  SeatState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct SeatState: Encodable {
    var seatList: [SeatInfo] = []
    var seatApplicationList: [SeatApplication] = []
    var mySeatApplicationId: String = ""
    
    var sentSeatInvitationMap: [String: SeatInvitation] = [:]
    var receivedSeatInvitation: SeatInvitation = SeatInvitation()
}

struct SeatApplication: Codable {
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
}

struct SeatInvitation: Codable {
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

struct SeatInfo: Codable {
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
    
    init(userInfo: User) {
        self.index = -1
        self.userId = userInfo.userId
        self.userName = userInfo.name
        self.avatarUrl = userInfo.avatarUrl
        
        self.isLocked = false
        self.isVideoLocked = false
        self.isAudioLocked = false
    }
    
}

extension SeatInfo: Equatable {}

extension SeatApplication: Equatable {}

extension SeatInvitation: Equatable {}

extension SeatInfo: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }

}

