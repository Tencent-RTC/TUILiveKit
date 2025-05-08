//
//  TUIGiftStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import RTCCommon

public class TUIGiftStore {
    public static let shared = TUIGiftStore()
    private init() {}
    public var giftList: [TUIGift] = []
    public let giftCacheService: GiftCacheService = GiftCacheService()
    public var giftDataMap: Observable<[String: TUIGiftData]> = Observable([:])
    public var likeDataMap: Observable<[String: TUILikeData]> = Observable([:])
}

