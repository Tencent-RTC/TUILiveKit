//
//  TUIGiftPresenter.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation
import TUICore

protocol TUIGiftPresenterDelegate: AnyObject {
    func onGiftDidSend(_ model: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int, isSuccess: Bool, message: String)
    func onLikeDidSend(sender: TUIGiftUser, isSuccess: Bool, message: String)
    func onReceiveGift(_ model: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser)
    func onReceiveLike(sender: TUIGiftUser)
}

class TUIGiftPresenter {
    var groupId: String = ""
    var msgService: TUIGiftIMService = TUIGiftIMService()
    weak var delegate: TUIGiftPresenterDelegate?

    static func defaultCreate(_ delegate: TUIGiftPresenterDelegate, groupId: String) -> TUIGiftPresenter {
        let service = TUIGiftPresenter()
        service.delegate = delegate
        service.groupId = groupId
        service.initIM()
        return service
    }

    private func initIM() {
        msgService = TUIGiftIMService.defaultCreate(self, groupID: groupId)
    }

    private func dealloc() {
        msgService.releaseResources()
    }
}

// MARK: Send Msg

extension TUIGiftPresenter {
    func sendGift(_ giftModel: TUIGift, receiver: TUIGiftUser, giftCount: Int) {
        let sender = TUIGiftUser()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        sender.level = "0"
        msgService.sendGiftMessage(giftModel,
                                   sender: sender,
                                   receiver: receiver,
                                   giftCount: giftCount) { [weak self] code, msg in
            guard let self = self else { return }
            let isSuccess = (code == 0)
            self.delegate?.onGiftDidSend(giftModel, sender: sender, receiver: receiver, giftCount: giftCount, isSuccess: isSuccess, message: msg)
            if isSuccess {
                let giftData = TUIGiftData(gift: giftModel, giftCount: giftCount, sender: sender, receiver: receiver)
                giftData.sender.userName = .meText
                TUIGiftStore.shared.giftData.value = giftData
            }
        }
    }

    func sendLike() {
        let sender = TUIGiftUser()
        sender.userId = TUILogin.getUserID() ?? ""
        sender.userName = TUILogin.getNickName() ?? ""
        sender.avatarUrl = TUILogin.getFaceUrl() ?? DEFAULT_AVATAR
        sender.level = "0"
        msgService.sendLikeMessage(sender: sender) { [weak self] code, msg in
            let isSuccess = (code == 0)
            self?.delegate?.onLikeDidSend(sender: sender, isSuccess: isSuccess, message: msg)
        }
    }
}

// MARK: TUIGiftIMServiceDelegate

extension TUIGiftPresenter: TUIGiftIMServiceDelegate {
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
