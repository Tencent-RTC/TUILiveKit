//
// TUIBarrageStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/1.
//

import Foundation

class TUIBarrageStore {
    static let shared = TUIBarrageStore()
    private init() {}
    var barrageMap: Observable<[String: TUIBarrage]> =  Observable([:])
    var ownerId: String = ""
}
