//
//  LikeManager.swift
//  TUILiveKit
//
//  Created by gg on 2025/1/7.
//

import Foundation
import ImSDK_Plus
import TUICore
import TUIGift

class LikeManager {
    private var lastSendDate: Date?
    private let DEFAULT_AVATAR = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png"
    private let roomId: String
    
    init(roomId: String) {
        self.roomId = roomId
    }
    
    func sendLike() {
        let sender = TUIGiftUser()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        sender.level = "0"
        
        TUIGiftStore.shared.likeDataMap.value = [roomId: TUILikeData(sender: sender)]
        
        let current = Date()
        if let lastSendDate = lastSendDate, current.timeIntervalSince(lastSendDate) < 2 {
            return
        }
        lastSendDate = current
        doSendLike(roomId: roomId, sender: sender)
    }
    
    func doSendLike(roomId: String, sender: TUIGiftUser) {
        Task {
            do {
                try await sendLikeMessage(roomId: roomId, sender:sender)
            } catch let err {
                
            }
        }
    }
    
    func sendLikeMessage(roomId: String, sender: TUIGiftUser) async throws {
        return try await withCheckedThrowingContinuation { continuation in
            do {
                let likeData = TUILikeData(sender: sender)
                let likeWrapper = TUILikeWrapper(businessID: "TUIGift_like", data: likeData)
                let encoder = JSONEncoder()
                let data = try encoder.encode(likeWrapper)
                V2TIMManager.sharedInstance().sendGroupCustomMessage(data, to: roomId, priority: .PRIORITY_LOW) {
                    continuation.resume()
                } fail: { code, message in
                    debugPrint("sendGroupCustomMessage failed. code:\(code), message:\(message ?? "")")
                    continuation.resume(throwing: InternalError(code: Int(code), message: message ?? ""))
                }
            } catch let err {
                continuation.resume(throwing: err)
            }
        }
    }
}
