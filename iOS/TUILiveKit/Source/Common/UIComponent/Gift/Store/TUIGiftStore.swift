//
//  TUIGiftStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation

class TUIGiftStore {
    static let shared = TUIGiftStore()
    private init() {}
    var giftData: Observable<TUIGiftData> = Observable(TUIGiftData())
    var likeData: Observable<TUILikeData> = Observable(TUILikeData())
}
