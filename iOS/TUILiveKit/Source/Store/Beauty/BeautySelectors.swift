//
//  BeautySelectors.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import Foundation

enum BeautySelectors {
    static let getBeautyState = Selector(keyPath: \OperationState.beautyState)
    static let getSmoothLevel = Selector.with(getBeautyState, keyPath: \.smoothLevel)
    static let getWhitenessLevel = Selector.with(getBeautyState, keyPath: \.whitenessLevel)
    static let getRuddyLevel = Selector.with(getBeautyState, keyPath: \.ruddyLevel)
}
