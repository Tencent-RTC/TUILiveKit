//
//  TUILikeButton.swift
//  TUIGift
//
//  Created by adamsfliu on 2025/5/27.
//

import UIKit
import TUICore
import RTCRoomEngine

private actor ActorCounter {
    private var count: UInt = 0
    
    func increment() {
        count += 1
    }
    
    func reset() {
        count = 0
    }
    
    func getCount() -> UInt {
        return count
    }
}

public class TUILikeButton: UIButton {
    
    private let roomId: String
    private let service: LikeService
    
    private let DEFAULT_AVATAR = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png"
    private var lastSendTime: TimeInterval = 0
    private let pendingCountActor = ActorCounter()
    private let minSendInterval: TimeInterval = 6.0
    private var afterSendLikeWorkItem: DispatchWorkItem?
    
    public init(roomId: String) {
        self.roomId = roomId
        self.service = LikeService(roomId: roomId)
        super.init(frame: .zero)
        setImage(internalImage("live_like_icon"), for: .normal)
        setImage(internalImage("live_like_icon"), for: .selected)
        
        addTarget(self, action: #selector(likeButtonClick(sender:)), for: .touchUpInside)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("\(type(of: self)) deinit")
    }
    
    @objc
    private func likeButtonClick(sender: UIButton) {
        let sender = TUIUserInfo()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        TUIGiftStore.shared.likeDataMap.value = [roomId: TUILikeData(sender: sender)]
        
        TUIGiftStore.shared.localLikeCount += 1
        Task {
            await pendingCountActor.increment()
        }

        let now = Date().timeIntervalSince1970
        let elapsed = now - lastSendTime
        
        afterSendLikeWorkItem?.cancel()
        
        if elapsed >= minSendInterval {
            sendLike()
            lastSendTime = now
        } else {
            afterSendLikeWorkItem = DispatchWorkItem(block: { [weak self] in
                guard let self = self else { return }
                self.sendLike()
                self.lastSendTime = Date().timeIntervalSince1970
            })
            if let workItem = afterSendLikeWorkItem {
                DispatchQueue.main.asyncAfter(deadline: .now() + minSendInterval, execute: workItem)
            }
        }
    }
    
    private func sendLike() {
        Task { [weak self] in
            guard let self = self else { return }
            let countToSend = await pendingCountActor.getCount()
            do {
                LiveKitLog.info("\(#file)", "\(#line)", "sendLike count: \(countToSend)")
                try await service.sendLike(count: countToSend)
                await pendingCountActor.reset()
            } catch let err as InternalError {
                LiveKitLog.error("\(#file)", "\(#line)", "sendLike failed:\(err.localizedMessage)")
                TUIGiftStore.shared.toastSubject.send(err.localizedMessage)
            }
        }
    }
}

