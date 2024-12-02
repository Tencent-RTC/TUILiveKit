//
//  BattleState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import RTCRoomEngine

let battleDuration: TimeInterval = 30
let battleRequestTimeout: TimeInterval = 10
let battleEndInfoDuration: TimeInterval = 5

typealias LSReceivedBattleRequest = (info: TUIBattleInfo, inviter: BattleUser, invitee: BattleUser)
typealias LSSentBattleRequest = (info: TUIBattleInfo?, inviteeIdList: [String])

struct LSBattleState {
    
    var battleId: String = ""
    var battleUsers: [BattleUser] = []
    var sentBattleRequest: LSSentBattleRequest = (nil, [])
    var receivedBattleRequest: LSReceivedBattleRequest?
    var durationCountDown: Int = 0
    var battleConfig: BattleConfig = BattleConfig()
    var needResponse: Bool = true
    
    var isInWaiting: Bool = false
    var isShowingStartView: Bool = false
    var isBattleRunning: Bool = false
    var isOnDisplayResult: Bool = false
    
    mutating func addSentBattleRequest(info: TUIBattleInfo, requestResultMap: [String: TUIBattleCode]) {
        let userIDs = sentBattleRequest.inviteeIdList
        requestResultMap.forEach { (key, code) in
            if code == .success {
                if !userIDs.contains(where: { $0 == key }) {
                    sentBattleRequest.inviteeIdList.append(key)
                }
            }
        }
        sentBattleRequest.info = info
    }
    
    mutating func clearSentBattleReuqest() {
        sentBattleRequest = (nil, [])
    }
}

struct BattleUser: Codable {
    var roomId: String
    var userId: String
    var avatarUrl: String
    var userName: String
    var score: UInt
    var ranking: Int
    
    init() {
        roomId = ""
        userId = ""
        avatarUrl = ""
        userName = ""
        score = 0
        ranking = 1
    }
    
    init(_ battleUser: TUIBattleUser) {
        self.roomId = battleUser.roomId
        self.userId = battleUser.userId
        self.userName = battleUser.userName
        self.avatarUrl = battleUser.avatarUrl
        self.score = battleUser.score
        ranking = 1
    }
}

extension BattleUser: Equatable {
    static func == (lhs: BattleUser, rhs: BattleUser) -> Bool {
        return lhs.userId == rhs.userId && lhs.roomId == rhs.roomId && lhs.score == rhs.score
    }
}

extension BattleUser: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(roomId)
    }
}

struct BattleConfig: Encodable {
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
