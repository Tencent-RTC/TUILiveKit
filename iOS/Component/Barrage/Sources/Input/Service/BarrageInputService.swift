//
//  BarrageInputService.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/13.
//

import ImSDK_Plus
import RTCRoomEngine

class BarrageInputService {
    let roomId: String
    
    private var imManager: V2TIMManager {
        V2TIMManager.sharedInstance()
    }
    
    init(roomId: String) {
        self.roomId = roomId
    }

    func sendBarrage(_ barrage: TUIBarrage) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            imManager.sendGroupTextMessage(barrage.content, to: roomId, priority: .PRIORITY_LOW) {
                continuation.resume()
            } fail: { code, message in
                continuation.resume(throwing: InternalError(error: TIMError(rawValue: Int(code)) ?? UnknownError(rawValue: Int(code)), message: message ?? ""))
            }
        }
    }
}
