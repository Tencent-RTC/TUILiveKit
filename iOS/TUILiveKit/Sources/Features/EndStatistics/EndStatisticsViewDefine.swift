//
//  AnchorDashboardDefine.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/17.
//

import Foundation

class AnchorEndStatisticsViewInfo {
    let roomId: String
    let liveDuration: Int
    var viewCount: Int
    let messageCount: Int
    let giftTotalCoins: Int
    let giftTotalUniqueSender: Int
    let likeTotalUniqueSender: Int
    
    init(roomId: String, liveDuration: Int, viewCount: Int, messageCount: Int, giftTotalCoins: Int, giftTotalUniqueSender: Int, likeTotalUniqueSender: Int) {
        self.roomId = roomId
        self.liveDuration = liveDuration
        self.viewCount = viewCount
        self.messageCount = messageCount
        self.giftTotalCoins = giftTotalCoins
        self.giftTotalUniqueSender = giftTotalUniqueSender
        self.likeTotalUniqueSender = likeTotalUniqueSender
    }
}

protocol AnchorEndStatisticsViewDelegate: AnyObject {
    func onCloseButtonClick()
}

protocol AudienceEndStatisticsViewDelegate: AnyObject {
    func onCloseButtonClick()
}
