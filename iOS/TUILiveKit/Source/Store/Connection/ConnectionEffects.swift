//
//  ConnectionEffects.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Combine
import Foundation
import RTCRoomEngine

class ConnectionEffects: Effects {
    typealias Environment = ServiceCenter
    
    let getRecommendedList = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: ConnectionActions.getRecommendedList)
            .flatMap { action in
                environment.connectionService.getRecommendedList(cursor: action.payload)
                    .map { (cursor, liveList) in
                        return ConnectionActions.updateRecommendedList(payload: (action.payload, cursor, liveList))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let requestConnection = Effect<Environment>.dispatchingMultiple({ actions, environment in
        actions
            .wasCreated(from: ConnectionActions.requestConnection)
            .flatMap { action in
                environment.connectionService.requestConnection(roomIdList: action.payload.0, extensionInfo: action.payload.1)
                    .map { result in
                        guard let store = environment.store else {
                            return []
                        }
                        var actions: [Action] = []
                        result.forEach { (key: String, value: TUIConnectionCode) in
                            if value == .success || value == .connecting {
                                var invitee = ConnectionUser()
                                invitee.roomId = key
                                invitee.connectionStatus = .inviting
                                actions.append(ConnectionActions.addSentConnectionInvitation(payload: invitee))
                            }
                            if value == .connectingOtherRoom{
                                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .errorConnectionDisableText)))
                            }
                            if value == .full{
                                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .errorConnectionMaxLimitText)))
                            }
                        }
                        return actions
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
                
            }
            .eraseToAnyPublisher()
    })
    
    let cancelRequest = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: ConnectionActions.cancelRequest)
            .flatMap { action in
                environment.connectionService.cancelRequest(roomIdList: action.payload)
                    .map {
                        guard let store = environment.store else {
                            return []
                        }
                        var actions:[Action] = []
                        action.payload.forEach { roomId in
                            actions.append(ConnectionActions.removeSentConnectionInvitation(payload: roomId))
                        }
                        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .operationSuccessful)))
                        return actions
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let accept = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: ConnectionActions.accept)
            .flatMap { action in
                environment.connectionService.accept(roomId: action.payload)
                    .map {
                        [
                            ConnectionActions.respondConnectionRequest(payload: (true, action.payload)),
                        ]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let reject = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: ConnectionActions.reject)
            .flatMap { action in
                environment.connectionService.reject(roomId: action.payload)
                    .map {
                        [
                            ConnectionActions.respondConnectionRequest(payload: (false, action.payload)),
                        ]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let disconnect = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: ConnectionActions.disconnect)
            .flatMap { action in
                environment.connectionService.disconnect()
                    .map {
                        let toastAction = ViewActions.toastEvent(payload: ToastInfo(message: .operationSuccessful))
                        environment.store?.dispatch(action: toastAction)
                        return ConnectionActions.onConnectionUserListChanged(payload: [])
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    
}

fileprivate extension String {
    static let operationSuccessful = localized("live.error.success")
    
    static let errorConnectionDisableText = localized("live.error.connectionDisable.connecting")
    static let errorConnectionMaxLimitText = localized("live.error.connectionDisable.maxLimit")
}

