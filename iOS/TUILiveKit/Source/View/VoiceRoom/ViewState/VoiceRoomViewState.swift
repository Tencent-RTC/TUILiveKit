//
//  VoiceRoomViewState.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/16.
//

import Foundation

// All View's state define
struct VoiceRoomViewState {
    var rootViewState = ViewState()
    var menu = MenuState()
}

struct MenuState {
    var menusButtons: [ButtonMenuInfo] = {
        let creator = BottomPopupListViewDataHelper()
        return creator.generateAudienceBottomMenuData()
    }()
}

struct VoiceRoomNavigationState {
    enum Router {
        case exit
        case main
        case audioEffectPanel
        case seatApplication
        case listMenu(_ menus: [ActionItem])
        case giftList
        case audienceList
        case musicListPanel
        case roomInfoPanel
        case systemImageSelection
    }
    
    var currentRouter: Router = .main
}

extension VoiceRoomNavigationState.Router: Equatable {
    static func ==(lhs: VoiceRoomNavigationState.Router, rhs: VoiceRoomNavigationState.Router) -> Bool {
        switch (lhs, rhs) {
        case (.exit, .exit),
            (.main, .main),
            (.audioEffectPanel, .audioEffectPanel),
            (.seatApplication, .seatApplication),
            (.giftList, .giftList),
            (.audienceList, .audienceList),
            (.musicListPanel, .musicListPanel),
            (.roomInfoPanel, .roomInfoPanel),
            (.systemImageSelection,.systemImageSelection):
            return true
        case let (.listMenu(l), .listMenu(r)):
            return l == r
        case (.exit, _),
            (.main, _),
            (.audioEffectPanel, _),
            (.seatApplication, _),
            (.giftList, _),
            (.listMenu, _),
            (.audienceList, _),
            (.musicListPanel,_),
            (.roomInfoPanel, _),
            (.systemImageSelection, _):
            return false
        }
    }
}
