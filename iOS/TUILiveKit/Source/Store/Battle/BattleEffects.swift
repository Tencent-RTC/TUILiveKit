//
//  BattleEffects.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import Combine
import RTCRoomEngine

class BattleEffects: Effects {
    typealias Environment = ServiceCenter
    
    let requestBattle = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: BattleActions.requestBattle)
            .flatMap { action in
                let config = TUIBattleConfig()
                config.duration = battleDuration
                config.needResponse = true
                config.extensionInfo = ""
                environment.store?.dispatch(action: BattleActions.setBattleConfig(payload: config))
                return environment.battleService.requestBattle(config: config, userIdList: action.payload.0, timeout: action.payload.1)
                    .map { (battleInfo, resultMap) in
                        guard let store = environment.store else { return []}
                        var actions: [Action] = []
                        resultMap.forEach { (key: String, value: TUIBattleCode) in
                            switch value {
                                case .success:
                                    var invitee = BattleUser()
                                    invitee.userId = key
                                    actions.append(BattleActions.addSentBattleInvitation(payload: invitee))
                                default:
                                    let error = InternalError(error: value, message: value.description)
                                    let action = environment.errorService.convert(error: error)
                                    actions.append(action)
                                    break
                            }
                        }
                        actions.append(BattleActions.setBattleId(payload: battleInfo.battleId))
                        return actions
                    }
                    .catch { error -> Just<[Action]> in
                        var actions: [Action] = []
                        let errorAction = environment.errorService.convert(error: error)
                        actions.append(errorAction)
                        actions.append(BattleActions.setIsInWaiting(payload: false))
                        return Just(actions)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let cancelRequest = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: BattleActions.cancelBattleRequest)
            .flatMap { action in
                
                let userList = environment.store?.selectCurrent(BattleSelectors.getSentBattleRequests).map { $0.userId }
                return environment.battleService.cancelBattleRequest(battleId: action.payload, userIdList: userList ?? [])
                    .map {[
                        BattleActions.clearSentBattleInvitation(),
                        BattleActions.setIsInWaiting(payload: false),]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let accept = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BattleActions.acceptBattle)
            .flatMap { action in
                environment.battleService.acceptBattle(battleId: action.payload)
                    .map {
                        BattleActions.responseBattleRequest()
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let reject = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BattleActions.rejectBattle)
            .flatMap { action in
                environment.battleService.rejectBattle(battleId: action.payload)
                    .map {
                        BattleActions.responseBattleRequest()
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let exit = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: BattleActions.exitBattle)
            .sink { action in
                environment.battleService.exitBattle(battleId: action.payload)
            }
    }
}


