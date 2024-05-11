//
//  State.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

import Foundation

// All room effect state define.
struct OperationState: Encodable {
    var mediaState = MediaState()
    var roomState = RoomState()
    var seatState = SeatState()
    var userState = UserState()
}

// All View's state define
struct GlobalViewState {
    var rootViewState = RootViewState()
    var menu = MenuState()
}

struct NavigationState {
    enum Router {
        case exit
        case main
        case audioEffectPanel
        case seatApplication
        case listMenu(_ menus: [ListMenuInfo])
        case giftList
        case audienceList
        case musicListPanel
    }
    
    var currentRouter: Router = .main
}

extension NavigationState.Router: Equatable {
    static func ==(lhs: NavigationState.Router, rhs: NavigationState.Router) -> Bool {
        switch (lhs, rhs) {
            case (.exit, .exit),
                (.main, .main),
                (.giftList, .giftList),
                (.audioEffectPanel, .audioEffectPanel),
                (.musicListPanel, .musicListPanel),
                (.seatApplication, .seatApplication):
                return true
            case let (.listMenu(l), .listMenu(r)):
                return l == r
            case (.exit, _),
                (.main, _),
                (.audioEffectPanel, _),
                (.giftList, _),
                (.seatApplication, _),
                (.listMenu, _),
                (.audienceList, _),
                (.musicListPanel,_):
                return false
        }
    }
}
