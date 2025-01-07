//
//  LikeManager.swift
//  TUILiveKit
//
//  Created by gg on 2025/1/7.
//

import Foundation
import ImSDK_Plus
import TUICore

class LikeManager {
    private var sendLikeDate: Date
    private var currentLikeCount: Int = 0
    private var sendLikeWorkItem: DispatchWorkItem?
    private let DEFAULT_AVATAR = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png"
    private let roomId: String
    
    init(roomId: String) {
        self.roomId = roomId
        self.sendLikeDate = Date(timeIntervalSinceNow: -1 * 60)
    }
    
    func sendLike() {
        let sender = TUIGiftUser()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        sender.level = "0"
        
        let maxLikeCount: Int = 20
        let maxDuration: Double = 5
        if currentLikeCount >= maxLikeCount {
            doSendLike(roomId: roomId, sender: sender)
            currentLikeCount = 0
            sendLikeDate = Date()
            return
        }
        let duration = -sendLikeDate.timeIntervalSinceNow
        if duration > maxDuration {
            doSendLike(roomId: roomId, sender: sender)
            currentLikeCount = 0
            sendLikeDate = Date()
        } else {
            currentLikeCount += 1
            let sender = TUIGiftUser()
            doSendLike(roomId: roomId, sender: sender)
            let delayInSeconds = maxDuration - duration
            sendLikeWorkItem?.cancel()
            let workItem = DispatchWorkItem { [weak self] in
                guard let self = self else { return }
                sendLike()
            }
            sendLikeWorkItem = workItem
            DispatchQueue.main.asyncAfter(deadline: .now() + delayInSeconds, execute: workItem)
        }
    }
    
    func doSendLike(roomId: String, sender: TUIGiftUser) {
        Task {
            do {
                try await sendLikeMessage(roomId: roomId, sender:sender)
                DispatchQueue.main.async {
                    TUIGiftStore.shared.likeDataMap.value = [roomId: TUILikeData(sender: sender)]
                }
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
                V2TIMManager.sharedInstance().sendGroupCustomMessage(data, to: roomId, priority: .PRIORITY_NORMAL) {
                    continuation.resume()
                } fail: { code, message in
                    debugPrint("sendGroupCustomMessage failed. code:\(code), message:\(message ?? "")")
                    guard let err = TIMError(rawValue: Int(code)) else {
                        continuation.resume(throwing: InternalError(error: TIMError.failed, message: ""))
                        return
                    }
                    continuation.resume(throwing: InternalError(error: err, message: message ?? ""))
                }
            } catch let err {
                continuation.resume(throwing: err)
            }
        }
    }
}
