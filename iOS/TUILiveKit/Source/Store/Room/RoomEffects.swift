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
                        return action.payload.nextActions + [
                            RoomActions.updateRoomInfo(payload: roomInfo),
                            RoomActions.setRoomCoverUrl(payload: roomState.coverURL),
                            RoomActions.setRoomCategory(payload: roomState.liveExtraInfo.category),
                            RoomActions.setRoomMode(payload: roomState.liveExtraInfo.liveMode),
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
                        environment.store?.dispatch(action: OperationActions.clearAllState())
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
                        return [
                            RoomActions.fetchRoomOwnerInfo(payload: roomInfo.ownerId),
                            RoomActions.updateRoomInfo(payload: roomInfo),
                        ] + action.payload.nextActions
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let leaveLive = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: RoomActions.leave)
            .flatMap { _ in
                environment.roomService.leave()
                    .map { _ in
                        environment.store?.dispatch(action: ViewActions.updateLiveStatus(payload: .none))
                        environment.store?.dispatch(action: OperationActions.clearAllState())
                        return [RoomActions.leaveSuccess(),
                                MediaActions.stopLocalPreview(),
                                MediaActions.cameraClosed(),
                                MediaActions.microphoneClosed(),
                        ]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchRoomOwnerInfo = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.fetchRoomOwnerInfo)
            .flatMap { action in
                environment.roomService.fetchRoomOwnerInfo(ownerId: action.payload)
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
    
    let fetchLiveInfo = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions.wasCreated(from: RoomActions.fetchLiveInfo)
            .flatMap { action in
                environment.roomService.fetchLiveInfo(roomId: action.payload)
                    .map { liveInfo in
                        if let categoryValue = liveInfo.categoryList.first?.intValue,
                           let category = LiveStreamCategory(rawValue: categoryValue) {
                            environment.store?.dispatch(action: RoomActions.updateRoomCategory(payload: category))
                        }
                        return [RoomActions.updateRoomCoverUrl(payload: liveInfo.coverUrl),
                                RoomActions.updateRoomMode(payload: liveInfo.isPublicVisible ? .public : .privacy),
                        ]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRoomSeatModeByAdmin = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.setRoomSeatModeByAdmin)
            .flatMap { action in
                environment.roomService.setRoomSeatModeByAdmin(action.payload)
                    .map { _ in
                        RoomActions.updateRoomSeatModeByAdmin(payload: action.payload)
                    }
                    .catch { error -> Just<Action> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRoomMode = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.setRoomMode)
            .flatMap { action in
                var liveInfo = TUILiveInfo()
                liveInfo.roomInfo.roomId = environment.store?.selectCurrent(RoomSelectors.getRoomId) ?? ""
                liveInfo.isPublicVisible = action.payload == .public
                return environment.roomService.setLiveInfo(liveInfo: liveInfo, modifyFlag: .publish)
                    .map { _ in
                        RoomActions.updateRoomMode(payload: action.payload)
                    }
                    .catch { error -> Just<Action> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRoomCategory = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.setRoomCategory)
            .flatMap { action in
                var liveInfo = TUILiveInfo()
                liveInfo.roomInfo.roomId = environment.store?.selectCurrent(RoomSelectors.getRoomId) ?? ""
                liveInfo.categoryList = [NSNumber(value: action.payload.rawValue)]
                return environment.roomService.setLiveInfo(liveInfo: liveInfo, modifyFlag: .category)
                    .map { _ in
                        RoomActions.updateRoomCategory(payload: action.payload)
                    }
                    .catch { error -> Just<Action> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRoomCoverUrl = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.setRoomCoverUrl)
            .flatMap { action in
                var liveInfo = TUILiveInfo()
                liveInfo.roomInfo.roomId = environment.store?.selectCurrent(RoomSelectors.getRoomId) ?? ""
                liveInfo.coverUrl = action.payload
                return environment.roomService.setLiveInfo(liveInfo: liveInfo, modifyFlag: .coverUrl)
                    .map { _ in
                        RoomActions.updateRoomCoverUrl(payload: action.payload)
                    }
                    .catch { error -> Just<Action> in
                        let action = ViewActions.toastEvent(payload: ToastInfo(message: error.message))
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRoomBackgroundUrl = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomActions.setRoomBackgroundUrl)
            .flatMap { action in
                return Just(RoomActions.updateRoomBackgroundUrl(payload: action.payload))
            }
            .eraseToAnyPublisher()
    }
}
