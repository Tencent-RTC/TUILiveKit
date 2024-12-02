//
//  BeautySelectors.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import Foundation

enum BeautySelectors {
    static let getSmoothLevel = Selector(keyPath: \BeautyState.smoothLevel)
    static let getWhitenessLevel = Selector(keyPath: \BeautyState.whitenessLevel)
    static let getRuddyLevel = Selector(keyPath: \BeautyState.ruddyLevel)
    static let isCloseBeauty = Selector.with(BeautySelectors.getSmoothLevel,
                                             BeautySelectors.getWhitenessLevel,
                                             BeautySelectors.getRuddyLevel) { smoothLevel, whitenessLevel, ruddyLevel in
        return (smoothLevel == 0) && (whitenessLevel == 0) && (ruddyLevel == 0)
    }
}
