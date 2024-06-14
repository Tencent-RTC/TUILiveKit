//
//  RoomEffects.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Combine
import Foundation
import RTCRoomEngine

class RoomEffects: Effects {
    typealias Environment = ServiceCenter

    let startLive = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: RoomActions.start)
            .flatMap { action in
                environment.roomService.start(roomInfo: action.payload.param)
                    .map { roomInfo in
                        guard let store = environment.store else {
                            let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                            let action = environment.errorService.convert(error: error)
                            return [action]
                        }
                        
                        store.dispatch(action: ViewActions.updateLiveStatus(payload: .pushing))
                        store.dispatch(action: RoomActions.updateRoomOwnerInfo(payload: store.userState.selfInfo))
                        
                        let roomState = store.selectCurrent(RoomSelectors.getRoomState)
                        let liveInfo = TUILiveInfo()
                        liveInfo.roomInfo.roomId = roomInfo.roomId
                        liveInfo.coverUrl = roomState.coverURL
                        liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode == .public
                        liveInfo.categoryList = [NSNumber(value: roomState.liveExtraInfo.category.rawValue)]
                        
                        return action.payload.nextActions + [
                            RoomActions.updateLiveInfo(payload: (liveInfo, .coverUrl)),
                            RoomActions.updateLiveInfo(payload: (liveInfo, .category)),
                            RoomActions.updateLiveInfo(payload: (liveInfo, .publish)),
                            RoomActions.joinSuccess(payload: roomInfo),
                        ]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }

    let stopLive = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: RoomActions.stop)
            .flatMap { _ in
                environment.roomService.stop()
                    .map {
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
                environment.roomService.join(roomId: action.payload.param)
                    .map { roomInfo in
                        environment.store?.dispatch(action: RoomActions.updateRoomOwnerInfo(payload: User(userId: roomInfo.ownerId)))
                        environment.store?.dispatch(action: ViewActions.updateLiveStatus(payload: .playing))
                        environment.store?.dispatch(action: RoomActions.fetchRoomOwnerInfo())
                        return [
                            RoomActions.joinSuccess(payload: roomInfo),
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
            .flatMap { _ in
                environment.roomService.leave()
                    .map { _ in
                        environment.store?.dispatch(action: ViewActions.updateLiveStatus(payload: .none))
                        environment.store?.dispatch(action: MediaActions.stopLocalPreview())
                        environment.store?.dispatch(action: MediaActions.cameraClosed())
                        environment.store?.dispatch(action: MediaActions.microphoneClosed())
                        return RoomActions.leaveSuccess()
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }

    let fetchRoomOwnerInfo = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.fetchRoomOwnerInfo)
            .flatMap { _ in
                environment.roomService.fetchRoomOwnerInfo()
                    .map { user in
                        RoomActions.updateRoomOwnerInfo(payload: user)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchRoomOwnerFansCount = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.fetchRoomOwnerFansCount)
            .flatMap { action in
                environment.userService.fetchFollowersCount(userId: action.payload)
                    .map { count in
                        RoomActions.updateRoomOwnerFansCount(payload: count)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchRoomInfo = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.fetchRoomInfo)
            .flatMap { action in
                environment.roomService.fetchRoomInfo()
                    .map { roomInfo in
                        RoomActions.updateRoomInfo(payload: roomInfo)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let updateLiveInfo = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.updateLiveInfo)
            .flatMap { action in
                environment.roomService.updateLiveInfo(liveInfo: action.payload.0, modifyFlag: action.payload.1)
                    .map { _ in
                        RoomActions.onUpdateLiveInfoSuccess()
                    }
                    .catch { error -> Just<Action> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
}
