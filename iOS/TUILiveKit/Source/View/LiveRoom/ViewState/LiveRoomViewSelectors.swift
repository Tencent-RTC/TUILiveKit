//
//  LiveRoomViewSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//


// Selector
enum LiveRoomViewSelectors {
    // View navigation
    static let routeStack = Selector(keyPath: \LiveRoomNavigationState.routeStack)
    static let currentRoute = Selector.with(routeStack) { stack in
        return stack.last
    }
    // Bottom menu
    static let getBottomMenuButtons = Selector(keyPath: \LiveRoomViewState.menu.menusButtons)
}
