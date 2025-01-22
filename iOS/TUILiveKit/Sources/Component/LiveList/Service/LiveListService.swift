//
//  LiveListService.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

import RTCRoomEngine
import Combine

class LiveListEffects: Effects {
    typealias Environment = LiveListService
    
    let getLiveInfoList = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: LiveListActions.getLiveInfoList)
            .flatMap { action in
                environment.getLiveList(cursor: action.payload)
                    .map { (result) in
                        return LiveListActions.updateLiveInfoList(payload: result)
                    }
                    .catch { error -> Just<Action> in
                        return Just(LiveListActions.toastError(payload: error))
                    }
            }
            .eraseToAnyPublisher()
    }
}

class LiveListService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    func getLiveList(cursor: String, count: Int = 20) -> AnyPublisher<LiveListResult, InternalError> {
        return Future<LiveListResult, InternalError> { [weak self] promise in
            guard let self = self else { return }
            guard let listManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                promise(.failure(InternalError(error:TUIError.failed, message: "getRoomListFailed")))
                return 
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { resCursor, liveInfoList in
                promise(.success((LiveListResult(isFirstFetch:cursor.isEmpty, cursor: resCursor, liveInfoList: liveInfoList))))
            } onError: { error, message in
                promise(.failure(InternalError(error: error, message: message)))
            }
        }.eraseToAnyPublisher()
    }
}
