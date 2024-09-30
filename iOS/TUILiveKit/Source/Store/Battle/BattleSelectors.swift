//
//  BattleSelectors.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
enum BattleSelectors {
    static let getBattleState = Selector(keyPath: \OperationState.battleState)
    static let getBattleId = Selector.with(getBattleState, keyPath: \.battleId)
    static let getBattleUsers = Selector.with(getBattleState, keyPath: \.battleUsers)
    static let getSentBattleRequests = Selector.with(getBattleState, keyPath: \.sentBattleRequests)
    static let getReceivedBattleRequest = Selector.with(getBattleState, keyPath: \.receivedBattleRequest)
    static let getIsBattleRunning = Selector.with(getBattleState, keyPath: \.isBattleRunning)
    static let getBattleConfig = Selector.with(getBattleState, keyPath: \.battleConfig)
    static let getBattleDuration = Selector.with(getBattleState, keyPath: \.battleConfig.duration)
    static let getIsInWaiting = Selector.with(getBattleState, keyPath: \.isInWaiting)
    static let getIsOnDisplayResult = Selector.with(getBattleState, keyPath: \.isOnDisplayResult)
}
