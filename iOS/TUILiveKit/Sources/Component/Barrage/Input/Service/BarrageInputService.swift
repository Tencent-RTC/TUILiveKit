//
//  BarrageInputService.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/13.
//

import ImSDK_Plus

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
            imManager.sendGroupTextMessage(barrage.content, to: roomId, priority: .PRIORITY_NORMAL) {
                continuation.resume()
            } fail: { code, message in
                if let err = TIMError(rawValue: Int(code)) {
                    let error = InternalError(error: err, message: message ?? "")
                    continuation.resume(throwing: error)
                } else {
                    continuation.resume(throwing: NSError(domain: message ?? "", code: Int(code)))
                }
            }
        }
    }
}
