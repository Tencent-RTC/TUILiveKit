//
//  LikeService.swift
//  TUILiveKit
//
//  Created by gg on 2025/1/7.
//

import Foundation
import RTCRoomEngine

class LikeService {
    private let giftManager: TUILiveGiftManager?
    private let roomId: String
    
    init(roomId: String) {
        self.roomId = roomId
        self.giftManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveGiftManager) as? TUILiveGiftManager
    }
    
    func sendLike(count: UInt) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let giftManager = giftManager else {
                continuation.resume(throwing: InternalError(code: TUIError.failed.rawValue, message: "gift service is not init"))
                return
            }
            giftManager.sendLike(roomId: roomId, count: count) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
    
    func getLikesCount() async throws -> UInt {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let giftManager = giftManager else {
                continuation.resume(throwing: InternalError(code: TUIError.failed.rawValue, message: "gift service is not init"))
                return
            }
            giftManager.getLikesCount(roomId: roomId) { totalLikesReceived in
                continuation.resume(returning: totalLikesReceived)
            } onError: { code, message in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
}
