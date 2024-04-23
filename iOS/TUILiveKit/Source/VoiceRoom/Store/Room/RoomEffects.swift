//
//  RoomEffects.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import Combine
import RTCRoomEngine

class RoomEffects: Effects {
    typealias Environment = ServiceCenter
    
    let startLive = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: RoomActions.start)
            .flatMap { action in
                environment.roomService.start(roomInfo: action.payload.param)
                    .map { roomInfo in
                        var actions = action.payload.nextActions
                        actions.append(RoomActions.joinSuccess(payload: roomInfo))
                        return actions
                    }
                    .catch { error -> Just<[Action]> in
                        // TODO: Here, the error requires special handling; a new room entry is already required.
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
                
            }
            .eraseToAnyPublisher()
    }
    
    let stopLive = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: RoomActions.stop)
            .flatMap { action in
                environment.roomService.stop()
                    .map {
                        environment.store?.dispatch(action: ViewActions.endView())
                        return RoomActions.stopSuccess()
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let joinLive = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: RoomActions.join)
            .flatMap { action in
                environment.roomService.join(roomId:action.payload.param)
                    .map { roomInfo in
                        return [
                            RoomActions.joinSuccess(payload: roomInfo),
                            ViewActions.endLoading(),
                        ] + action.payload.nextActions
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let leaveLive = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: RoomActions.leave)
            .flatMap { action in
                environment.roomService.leave()
                    .map { roomInfo in
                        environment.store?.dispatch(action: NavigatorActions.navigatorTo(payload: .exit))
                        return RoomActions.leaveSuccess()
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
}
