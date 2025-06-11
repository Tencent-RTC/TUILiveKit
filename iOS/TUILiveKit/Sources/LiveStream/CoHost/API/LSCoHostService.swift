//
//  LSCoHostService.swift
//  AFNetworking
//
//  Created by aby on 2024/11/18.
//

import Foundation
import RTCRoomEngine
import LiveStreamCore
import TUILiveComponent

protocol LSCoHostService {
    func fetchRecommendedList(cursor: String, count: Int) async throws -> (String, [TUILiveInfo])
}

class LSCoHostServiceImpl {
    let roomEngine: TUIRoomEngine = TUIRoomEngine.sharedInstance()
    var connectionManager: TUILiveConnectionManager {
        return roomEngine.getLiveConnectionManager()
    }
    
    func addConnectionObserver(_ observer: TUILiveConnectionObserver) {
        connectionManager.addObserver(observer)
    }
    
    func removeConnectionObserver(_ observer: TUILiveConnectionObserver) {
        connectionManager.removeObserver(observer)
    }
}

extension LSCoHostServiceImpl: LSCoHostService {
    
    func fetchRecommendedList(cursor: String, count: Int) async throws -> (String, [TUILiveInfo]) {
        return try await withUnsafeThrowingContinuation { [weak self] continuation  in
            guard let self = self, let listManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                continuation.resume(throwing: InternalError(code: ErrorLocalized.generalErrorCode,
                                                            message: "get LiveListManager error"))
                return
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { responseCursor, responseLiveList in
                continuation.resume(returning: (responseCursor, responseLiveList))
            } onError: { error, message in
                continuation.resume(throwing: InternalError(code: error.rawValue, message: message))
            }
        }
    }
}

