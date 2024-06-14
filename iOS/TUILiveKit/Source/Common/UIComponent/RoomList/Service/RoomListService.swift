//
//  RoomListService.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

import RTCRoomEngine
import Combine

class RoomListEffects: Effects {
    typealias Environment = RoomListService
    
    let getRoomInfoList = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: RoomListActions.getRoomInfoList)
            .flatMap { action in
                environment.getRoomList(cursor: action.payload)
                    .map { (cursor, roomInfoList) in
                        let result = RoomListResult(cursor: cursor, roomInfoList: roomInfoList)
                        return RoomListActions.updateRoomInfoList(payload: result)
                    }
                    .catch { error -> Just<Action> in
                        return Just(RoomListActions.toastError(payload: error))
                    }
            }
            .eraseToAnyPublisher()
    }
}

class RoomListService {
    private let listManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveListManager)
    
    func getRoomList(cursor: String, count: Int = 20) -> AnyPublisher<(String, [TUILiveInfo]), InternalError> {
        return Future<(String,[TUILiveInfo]), InternalError> { [weak self] promise in
            guard let self = self else { return }
            guard let listManager = listManager as? TUILiveListManager else {
                promise(.failure(InternalError(error:TUIError.failed, message: "getRoomListFailed")))
                return 
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { cursor, liveInfoList in
                promise(.success((cursor, liveInfoList)))
            } onError: { error, message in
                promise(.failure(InternalError(error: error, message: message)))
            }
        }.eraseToAnyPublisher()
    }
}
