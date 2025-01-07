//
//  GiftManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation
import TUICore

protocol GiftManagerDelegate: AnyObject {
    func onReceiveGift(_ model: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser)
    func onReceiveLike(sender: TUIGiftUser)
}

class GiftManager {
    weak var delegate: GiftManagerDelegate?
    
    private let DEFAULT_AVATAR = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png"
    private let roomId: String
    private let imService: TUIGiftIMService
    
    init(roomId: String, delegate: GiftManagerDelegate? = nil) {
        self.roomId = roomId
        self.delegate = delegate
        self.imService = TUIGiftIMService(roomId: roomId)
        imService.delegate = self
    }
}

// MARK: Send Msg

extension GiftManager {
    func sendGift(_ giftModel: TUIGift, receiver: TUIGiftUser, giftCount: Int, completion: @escaping (Int, String) -> ()) {
        let sender = TUIGiftUser()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        sender.level = "0"
        imService.sendGiftMessage(giftModel,
                                   sender: sender,
                                   receiver: receiver,
                                   giftCount: giftCount) { [weak self] code, msg in
            guard let self = self else { return }
            completion(code, msg)
            if code == 0 {
                let giftData = TUIGiftData(gift: giftModel, giftCount: giftCount, sender: sender, receiver: receiver)
                giftData.sender.userName = .meText
                TUIGiftStore.shared.giftDataMap.value = [roomId: giftData]
            }
        }
    }
}

// MARK: TUIGiftIMServiceDelegate

extension GiftManager: TUIGiftIMServiceDelegate {
    func onReceiveGiftMessage(_ giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, count: Int) {
        delegate?.onReceiveGift(giftModel, giftCount: count, sender: sender, receiver: receiver)
    }

    func onReceiveLikeMessage(sender: TUIGiftUser) {
        delegate?.onReceiveLike(sender: sender)
    }
}

private extension String {
    static var meText: String {
        localized("live.barrage.me")
    }
}
