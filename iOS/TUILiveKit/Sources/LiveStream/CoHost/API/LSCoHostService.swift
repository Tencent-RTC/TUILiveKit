//
//  LSCoHostService.swift
//  AFNetworking
//
//  Created by aby on 2024/11/18.
//

import Foundation
import RTCRoomEngine
import LiveStreamCore

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
            guard let self = self else { return }
            guard let listManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                continuation.resume(throwing: InternalError(error:TUIError.failed, message: String.localized("live.error.failed")))
                return
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { responseCursor, responseLiveList in
                let liveList = responseLiveList.filter { info in
                    return LiveIdentityGenerator.shared.getIDType(info.roomInfo.roomId) == .live
                }
                continuation.resume(returning: (responseCursor, liveList))
            } onError: { error, message in
                continuation.resume(throwing: InternalError(error: error, message: message))
            }
        }
    }
}

