//
//  NavigationState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import Foundation


// Selector
enum ViewSelectors {
    // View navigation
    private static let getRouteState = Selector(keyPath: \NavigationState.currentRouter)
    static let currentRouterSelector = Selector.with(getRouteState) { $0 }
    // Bottom menu
    static let getBottomMenuButtons = Selector(keyPath: \GlobalViewState.menu.menusButtons)
}
