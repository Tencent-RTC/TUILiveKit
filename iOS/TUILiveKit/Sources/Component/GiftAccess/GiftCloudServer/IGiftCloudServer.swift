//
//  IGiftCloudServer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import TUILiveComponent

protocol IGiftCloudServer {
    func rechargeBalance(callback: @escaping (TUIGiftServerError, Int) -> Void)
    func queryBalance(callback: @escaping (TUIGiftServerError, Int) -> Void)
    func queryGiftInfoList(callback: @escaping (TUIGiftServerError, [TUIGift]) -> Void)
    func sendGift(sender: String, receiver: String,
                  giftModel: TUIGift, giftCount: Int,
                  callback: @escaping (TUIGiftServerError, Int) -> Void)
}

enum TUIGiftServerError: Int {
    case noError = 0
    case operationFailed = -1
    case paramError = -2
    case balanceInsufficient = -3
}
