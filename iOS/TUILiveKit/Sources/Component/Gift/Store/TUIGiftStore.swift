//
//  TUIGiftStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

public class TUIGiftStore {
    public static let shared = TUIGiftStore()
    private init() {}
    
    public let giftCacheService: GiftCacheService = GiftCacheService()
    
    public var giftListMap: Observable<[String: [TUIGiftInfo]]> = Observable([:])
    public var giftDataMap: Observable<[String: TUIGiftData]> = Observable([:])
    public var likeDataMap: Observable<[String: TUILikeData]> = Observable([:])
    public var localLikeCount: Int = 0
    let toastSubject = PassthroughSubject<String, Never>()
    
    public func reset() {
        giftListMap = Observable([:])
        giftDataMap = Observable([:])
        likeDataMap = Observable([:])
        localLikeCount = 0
    }
}
