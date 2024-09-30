//
//  BattleReducer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
let battleReducer = Reducer<BattleState>(
    ReduceOn(BattleActions.onBattleStarted) { state, action in
        var battleUsers = action.payload.inviteeList.map { battleUser in
            return BattleUser(battleUser)
        }
        battleUsers.append(BattleUser(action.payload.inviter))
        state.battleId = action.payload.battleId
        state.battleUsers = battleUsers
        state.isBattleRunning = true
        state.battleConfig = BattleConfig(action.payload.config)
        state.isInWaiting = false
    },
    ReduceOn(BattleActions.onBattleEnded) { state, action in
        state.sentBattleRequests = []
        state.receivedBattleRequest = nil
        state.isBattleRunning = false
    },
    ReduceOn(BattleActions.onUserJoinBattle) { state, action in
        if !state.battleUsers.contains(where: {$0.userId == action.payload.userId}) {
            state.battleUsers.append(BattleUser(action.payload))
        }
    },
    ReduceOn(BattleActions.onUserExitBattle) { state, action in
        if state.battleUsers.count == 2 { return }
        if state.battleUsers.contains(where: {$0.userId == action.payload.userId}) {
            state.battleUsers.removeAll(where: {$0.userId == action.payload.userId})
        }
        
        var battleUsers = state.battleUsers
        state.battleUsers = battleUsers
            .sorted(by: { $0.score > $1.score })
            .enumerated()
            .map { (index, user) in
                var updatedUser = user
                if index > 0 && user.score == state.battleUsers[index - 1].score {
                    updatedUser.ranking = state.battleUsers[index - 1].ranking
                } else {
                    updatedUser.ranking = index + 1
                }
                return updatedUser
            }
    },
    ReduceOn(BattleActions.onBattleScoreChanged) { state, action in
        state.battleUsers = action.payload.map { BattleUser($0) }
            .sorted(by: { $0.score > $1.score })
            .enumerated()
            .map { (index, user) in
                var updatedUser = user
                if index > 0 && user.score == state.battleUsers[index - 1].score {
                    updatedUser.ranking = state.battleUsers[index - 1].ranking
                } else {
                    updatedUser.ranking = index + 1
                }
                return updatedUser
            }
    },
    ReduceOn(BattleActions.onBattleRequestReceived) { state, action in
        state.battleId = action.payload.0
        state.receivedBattleRequest = BattleUser(action.payload.1)
    },
    ReduceOn(BattleActions.onBattleRequestCancelled) { state, action in
        state.receivedBattleRequest = nil
    },
    ReduceOn(BattleActions.onBattleRequestTimeout) { state, action in
        state.receivedBattleRequest = nil
        state.isInWaiting = false
        
        if state.sentBattleRequests.contains(where: {$0.userId == action.payload.1.userId}) {
            state.sentBattleRequests.removeAll(where: {$0.userId == action.payload.1.userId})
        }
    },
    ReduceOn(BattleActions.onBattleRequestAccept) { state, action in
        state.receivedBattleRequest = nil
    },
    ReduceOn(BattleActions.onBattleRequestReject) { state, action in
        state.sentBattleRequests.removeAll(where: { $0.userId == action.payload.userId })
        if state.sentBattleRequests.isEmpty {
            state.isInWaiting = false
        }
    },
    ReduceOn(BattleActions.addSentBattleInvitation) { state, action in
        if !state.sentBattleRequests.contains(where: {$0.userId == action.payload.userId}) {
            state.sentBattleRequests.append(action.payload)
        }
    },
    ReduceOn(BattleActions.responseBattleRequest) { state, action in
        state.receivedBattleRequest = nil
    },
    ReduceOn(BattleActions.setBattleConfig) { state, action in
        state.battleConfig = BattleConfig(action.payload)
    },
    ReduceOn(BattleActions.setBattleId) { state, action in
        state.battleId = action.payload
    },
    ReduceOn(BattleActions.clearBattleUsers) { state, action in
        state.battleUsers = []
    },
    ReduceOn(BattleActions.setIsInWaiting) { state, action in
        state.isInWaiting = action.payload
    },
    ReduceOn(BattleActions.setIsOnDisplayResult) { state, action in
        state.isOnDisplayResult = action.payload
        if !action.payload {
            state.battleId = ""
        }
    }
)
