//
//  TUIGiftData.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2025/6/16.
//

import Foundation
import RTCRoomEngine
import TUICore

public class TUIGiftData {
    public var giftCount: Int = 0
    public var giftInfo: TUIGiftInfo = TUIGiftInfo()
    public var sender: TUIUserInfo = TUIUserInfo()
    
    public var isAdvanced: Bool {
        giftInfo.resourceUrl.count > 0
    }
    
    public init() {}
    
    public init(_ giftCount: Int, giftInfo: TUIGiftInfo,  sender: TUIUserInfo) {
        self.giftCount = giftCount
        self.giftInfo = giftInfo
        self.sender = sender
    }
}

extension TUIUserInfo {
    var isSelf: Bool {
        userId == TUILogin.getUserID()
    }
}
