//
//  RoomListSelectors.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Foundation

enum RoomListSelectors {
    // View navigation
    private static let getRouteState = Selector(keyPath: \RoomListNavigationState.currentRouter)
    static let getCurrentRouter = Selector.with(getRouteState) { $0 }
    
    static let getRoomInfoList = Selector(keyPath: \RoomListState.roomInfoListResult)
}
