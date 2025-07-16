//
//  AudienceBattleState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import RTCRoomEngine

let audienceBattleDuration: TimeInterval = 30
let audienceBattleRequestTimeout: TimeInterval = 10
let audienceBattleEndInfoDuration: TimeInterval = 5

typealias AudienceReceivedBattleRequest = (battleId: String, inviter: AudienceBattleUser)

struct AudienceBattleState {
    
    var battleId: String = ""
    var battleUsers: [AudienceBattleUser] = []
    var receivedBattleRequest: AudienceReceivedBattleRequest?
    var durationCountDown: Int = 0
    var battleConfig: AudienceBattleConfig = AudienceBattleConfig()
    var needResponse: Bool = true
    
    var isInWaiting: Bool = false
    var isShowingStartView: Bool = false
    var isBattleRunning: Bool = false
    var isOnDisplayResult: Bool = false
}

struct AudienceBattleUser: Codable {
    var roomId: String
    var userId: String
    var avatarUrl: String
    var userName: String
    var score: UInt
    var ranking: Int
    var rect: CGRect
    
    init() {
        roomId = ""
        userId = ""
        avatarUrl = ""
        userName = ""
        score = 0
        ranking = 1
        rect = .zero
    }
    
    init(_ battleUser: TUIBattleUser) {
        self.roomId = battleUser.roomId
        self.userId = battleUser.userId
        self.userName = battleUser.userName
        self.avatarUrl = battleUser.avatarUrl
        self.score = battleUser.score
        ranking = 1
        rect = .zero
    }
}

extension AudienceBattleUser: Equatable {
    static func == (lhs: AudienceBattleUser, rhs: AudienceBattleUser) -> Bool {
        return lhs.userId == rhs.userId && lhs.roomId == rhs.roomId && lhs.score == rhs.score
    }
}

extension AudienceBattleUser: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(roomId)
    }
}

struct AudienceBattleConfig: Encodable {
    var duration: TimeInterval
    var needResponse: Bool
    var extensionInfo: String
    
    init() {
        duration = 30
        needResponse = true
        extensionInfo = ""
    }
    
    init(_ config: TUIBattleConfig) {
        self.duration = config.duration
        self.needResponse = config.needResponse
        self.extensionInfo = config.extensionInfo
    }
}
