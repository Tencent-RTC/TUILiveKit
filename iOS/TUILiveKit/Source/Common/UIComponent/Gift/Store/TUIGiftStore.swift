//
//  TUIGiftStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import RTCCommon

class TUIGiftStore {
    static let shared = TUIGiftStore()
    private init() {
        giftCloudServer.queryGiftInfoList { [weak self] error, giftList in
            guard let self = self else { return }
            if error == .noError {
                self.giftList = giftList
            }
        }
    }
    var giftList: [TUIGift] = []
    let giftCloudServer: IGiftCloudServer = GiftCloudServer()
    let giftCacheService: GiftCacheService = GiftCacheService()
    var giftDataMap: Observable<[String: TUIGiftData]> = Observable([:])
    var likeDataMap: Observable<[String: TUILikeData]> = Observable([:])
}
