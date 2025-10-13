//
//  LikeButton.swift
//  TUIGift
//
//  Created by adamsfliu on 2025/5/27.
//

import AtomicXCore
import TUICore
import UIKit

public class LikeButton: UIButton {
    private let liveId: String
    private var store: LikeStore {
        LikeStore.create(liveID: liveId)
    }
    
    private let DEFAULT_AVATAR = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png"
    private var lastSendTime: TimeInterval = 0
    private var pendingCount: UInt = 0
    private let minSendInterval: TimeInterval = 6.0
    private var afterSendLikeWorkItem: DispatchWorkItem?
    
    public init(roomId: String) {
        self.liveId = roomId
        super.init(frame: .zero)
        setImage(internalImage("live_like_icon"), for: .normal)
        setImage(internalImage("live_like_icon"), for: .selected)
        
        addTarget(self, action: #selector(likeButtonClick(sender:)), for: .touchUpInside)
    }
    
    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("\(type(of: self)) deinit")
    }
    
    @objc
    private func likeButtonClick(sender: UIButton) {
        pendingCount += 1

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
        store.sendLike(count: pendingCount) { [weak self] result in
            guard let self = self else { return }
            switch result {
            case .success():
                pendingCount = 0
            case .failure(let error):
                let err = InternalError(code: error.code, message: error.message)
                GiftManager.shared.toastSubject.send(err.localizedMessage)
            }
        }
    }
}
