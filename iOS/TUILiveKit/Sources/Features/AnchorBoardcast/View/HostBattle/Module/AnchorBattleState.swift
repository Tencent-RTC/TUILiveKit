//
//  BattleState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import RTCRoomEngine

let anchorBattleDuration: TimeInterval = 30
let anchorBattleRequestTimeout: TimeInterval = 10
let anchorBattleEndInfoDuration: TimeInterval = 5

typealias AnchorReceivedBattleRequest = (battleId: String, inviter: AnchorBattleUser)

struct AnchorBattleState {
    
    var battleId: String = ""
    var battleUsers: [AnchorBattleUser] = []
    var receivedBattleRequest: AnchorReceivedBattleRequest?
    var durationCountDown: Int = 0
    var battleConfig: AnchorBattleConfig = AnchorBattleConfig()
    var needResponse: Bool = true
    
    var isInWaiting: Bool = false
    var isShowingStartView: Bool = false
    var isBattleRunning: Bool = false
    var isOnDisplayResult: Bool = false
}

struct AnchorBattleUser: Codable {
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

extension AnchorBattleUser: Equatable {
    static func == (lhs: AnchorBattleUser, rhs: AnchorBattleUser) -> Bool {
        return lhs.userId == rhs.userId && lhs.roomId == rhs.roomId && lhs.score == rhs.score
    }
}

extension AnchorBattleUser: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(roomId)
    }
}

struct AnchorBattleConfig: Encodable {
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
