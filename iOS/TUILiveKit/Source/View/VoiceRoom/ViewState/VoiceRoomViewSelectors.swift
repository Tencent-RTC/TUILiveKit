//
//  VoiceRoomNavigationState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import Foundation

// Selector
enum VoiceRoomViewSelectors {
    // Bottom menu
    static let getBottomMenuButtons = Selector(keyPath: \VoiceRoomViewState.menu.menusButtons)
}
