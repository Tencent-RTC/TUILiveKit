//
//  TUIGiftIMService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation
import ImSDK_Plus
import TUICore

typealias TUIGiftIMSendBlock = ((Int, String) -> Void)?

protocol TUIGiftIMServiceDelegate: AnyObject {
    func onReceiveGiftMessage(_ giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, count: Int)
    func onReceiveLikeMessage(sender: TUIGiftUser)
}

class TUIGiftIMService: NSObject {
    weak var delegate: TUIGiftIMServiceDelegate?
    
    private weak var imManager: V2TIMManager?
    private let roomId: String
    
    init(roomId: String, delegate: TUIGiftIMServiceDelegate? = nil) {
        self.delegate = delegate
        self.roomId = roomId
        super.init()
        imManager = V2TIMManager.sharedInstance()
        imManager?.addSimpleMsgListener(listener: self)
    }
}

// MARK: Send Msg

extension TUIGiftIMService {
    private func getGiftData(_ giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int) -> TUIGiftData {
        return TUIGiftData(gift:giftModel, giftCount: giftCount, sender: sender, receiver: receiver)
    }
    private func getLikeData(sender: TUIGiftUser) -> TUILikeData{
        return TUILikeData(sender: sender)
    }

    func sendGiftMessage(_ giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int, callback: TUIGiftIMSendBlock) {
        do {
            let giftData = getGiftData(giftModel, sender: sender, receiver: receiver, giftCount: giftCount)
            let giftWrapper = TUIGiftWrapper(businessID: "TUIGift", data: giftData)
            let encoder = JSONEncoder()
            let data = try encoder.encode(giftWrapper)
            imManager?.sendGroupCustomMessage(data, to: roomId, priority: .PRIORITY_NORMAL) {
                callback?(0, "send gift message success.")
            } fail: { code, message in
                debugPrint("sendGroupCustomMessage failed. code:\(code), message:\(message ?? "")")
                callback?(Int(code), message ?? "")
            }
        } catch {
            debugPrint("Encoding TUIBarrage failed. error:\(error)")
        }
    }

    func sendLikeMessage(sender: TUIGiftUser,callback: TUIGiftIMSendBlock) {
        do {
            let likeData = getLikeData(sender: sender)
            let likeWrapper = TUILikeWrapper(businessID: "TUIGift_like", data: likeData)
            let encoder = JSONEncoder()
            let data = try encoder.encode(likeWrapper)
            imManager?.sendGroupCustomMessage(data, to: roomId, priority: .PRIORITY_NORMAL) {
                callback?(0, "send like message success.")
            } fail: { code, message in
                debugPrint("sendGroupCustomMessage failed. code:\(code), message:\(message ?? "")")
                callback?(Int(code), message ?? "")
            }
        } catch {
            debugPrint("Encoding TUIBarrage failed. error:\(error)")
        }
    }
}

// MARK: TUIGiftIMServiceDelegate CallBack Handling

extension TUIGiftIMService {
    private func onReceiveGift(_ giftData: TUIGiftData) {
        delegate?.onReceiveGiftMessage(giftData.gift, sender: giftData.sender, receiver: giftData.receiver, count: giftData.giftCount)
    }

    private func onReceiveLike(_ likeData: TUILikeData) {
        delegate?.onReceiveLikeMessage(sender: likeData.sender)
    }
}

// MARK: V2TIMSimpleMsgListener

extension TUIGiftIMService: V2TIMSimpleMsgListener {
    func onRecvGroupCustomMessage(_ msgID: String, groupID: String, sender info: V2TIMGroupMemberInfo, customData data: Data!) {
        guard roomId == groupID else { return }
        let decoder = JSONDecoder()
        do {
            let giftWrapper = try decoder.decode(TUIGiftWrapper.self, from: data)
            if giftWrapper.businessID == "TUIGift" {
                let giftData = giftWrapper.data
                onReceiveGift(giftData)
            }
        } catch {
            guard let likeWrapper = try? decoder.decode(TUILikeWrapper.self, from: data) else { return }
            if likeWrapper.businessID == "TUIGift_like" {
                let likeData = likeWrapper.data
                onReceiveLike(likeData)
            }
            print("Decoding TUIBarrage failed. error:\(error)")
        }
    }
}

