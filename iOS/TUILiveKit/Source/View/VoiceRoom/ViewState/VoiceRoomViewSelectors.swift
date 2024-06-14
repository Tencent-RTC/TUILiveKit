//
//  VoiceRoomNavigationState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import Foundation


// Selector
enum VoiceRoomViewSelectors {
    // View navigation
    private static let getRouteState = Selector(keyPath: \VoiceRoomNavigationState.currentRouter)
    static let getCurrentRouter = Selector.with(getRouteState) { $0 }
    // Bottom menu
    static let getBottomMenuButtons = Selector(keyPath: \VoiceRoomViewState.menu.menusButtons)
}
