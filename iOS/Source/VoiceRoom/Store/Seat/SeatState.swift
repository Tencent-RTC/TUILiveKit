//
//  SeatState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine

struct SeatState: Encodable {
    var seatList: [SeatInfo] = [
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
        SeatInfo(),
    ]
    
    var seatApplications: [SeatApplication] = []
    var seatApplicationUserMap: [String: User] = [:]
    
    var mySeatApplicationId: String = ""
}

struct SeatApplication: Codable {
    var id: String
    var userId: String
    var content: String
    var timestamp: UInt
    
    init(request: TUIRequest) {
        self.id = request.requestId
        self.userId = request.userId
        self.content = request.content
        self.timestamp = request.timestamp
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
}

extension SeatInfo: Equatable {}


