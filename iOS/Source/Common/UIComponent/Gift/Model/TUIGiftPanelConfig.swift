//
//  TUIGiftPanelConfig.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation

class TUIGiftPanelConfig {
    var rows: Int = 2
    var itemSize: CGSize = CGSize(width: 74, height: 74)
    var giftDataSource: [TUIGift] = []
    
    static func defaultCreate() -> TUIGiftPanelConfig {
        let config = TUIGiftPanelConfig()
        config.rows = 2
        config.itemSize = CGSize(width: 74, height: 74 + 53)
        config.giftDataSource = []
        return config
    }
}

