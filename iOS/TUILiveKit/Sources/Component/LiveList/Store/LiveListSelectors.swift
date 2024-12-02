//
//  LiveListSelectors.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Foundation

enum LiveListSelectors {
    // View navigation
    private static let getRouteState = Selector(keyPath: \LiveListNavigationState.currentRouter)
    static let getCurrentRouter = Selector.with(getRouteState) { $0 }
    
    static let getLiveInfoList = Selector(keyPath: \LiveListState.liveInfoListResult)
}
