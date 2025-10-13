//
//  TUIGiftData.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2025/6/16.
//

import AtomicXCore
import Combine
import Foundation
import RTCRoomEngine
import TUICore

public class TUIGiftData {
    public let giftCount: UInt8
    public let giftInfo: Gift
    public let sender: LiveUserInfo

    public var isAdvanced: Bool {
        giftInfo.resourceURL.count > 0
    }

    public init(_ giftCount: UInt8, giftInfo: Gift, sender: LiveUserInfo) {
        self.giftCount = giftCount
        self.giftInfo = giftInfo
        self.sender = sender
    }
}

extension LiveUserInfo {
    var isSelf: Bool {
        userID == TUILogin.getUserID()
    }
}

class GiftManager {
    static let shared = GiftManager()
    private init() {}

    let toastSubject = PassthroughSubject<String, Never>()
    let giftCacheService = GiftCacheService()
}

extension LiveUserInfo {
    static var selfInfo: LiveUserInfo {
        let selfUserInfo = TUIRoomEngine.getSelfInfo()
        var user = LiveUserInfo()
        user.userID = selfUserInfo.userId
        user.userName = selfUserInfo.userName
        user.avatarURL = selfUserInfo.avatarUrl
        return user
    }
}
