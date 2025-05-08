//
//  LiveListService.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

import RTCRoomEngine
import Combine
import RTCCommon
import TUILiveResources

class LiveListService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    func getLiveList(cursor: String, count: Int = 20) async throws -> LiveListResult {
        return try await withCheckedThrowingContinuation { continuation in
            guard let listManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                continuation.resume(throwing: InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveListManager error"))
                return
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { resCursor, tuiLiveInfoList in
                let liveInfoList = tuiLiveInfoList.map { tuiLiveInfo in
                    LiveInfo(tuiLiveInfo: tuiLiveInfo)
                }
                continuation.resume(returning: LiveListResult(isFirstFetch:cursor.isEmpty, cursor: resCursor, liveInfoList: liveInfoList))
            } onError: { error, message in
                continuation.resume(throwing: InternalError(code: error.rawValue, message: message))
            }
        }
    }
}
