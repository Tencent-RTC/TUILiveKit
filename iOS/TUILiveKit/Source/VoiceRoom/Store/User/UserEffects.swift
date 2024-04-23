//
//  UserEffects.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//
import Combine
import RTCRoomEngine

class UserEffects: Effects {
    typealias Environment = ServiceCenter
    
    let fetchUserInfo = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: UserActions.fetchUserInfo)
            .flatMap { action in
                environment.userService.fetchUserInfo(action.payload.param)
                    .map { user in
                        var actionTemplates = action.payload.nextActionTemplates.map { template in
                            return template(payload: user)
                        }
                        return actionTemplates as [Action]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
                
            }
            .eraseToAnyPublisher()
    }
    
    let fetchUserList = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.fetchUserList)
            .flatMap { action in
                environment.userService.fetchUserList()
                    .map { userList in
                        return UserActions.updateUserList(payload: userList)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchRoomOwnerInfo = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.fetchRoomOwnerInfo)
            .flatMap { action in
                environment.userService.fetchRoomOwnerInfo()
                    .map { user in
                        return UserActions.updateRoomOwnerInfo(payload: user)
                }
                .catch { error -> Just<Action> in
                    let action = environment.errorService.convert(error: error)
                    return Just(action)
                }
            }
            .eraseToAnyPublisher()
    }

}
