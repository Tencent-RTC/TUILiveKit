//
//  LiveRoomViewReducer.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//

import Foundation

let liveRoomNavigationReducer = Reducer<LiveRoomNavigationState>(
    ReduceOn(LiveRoomNavigatorActions.navigate, reduce: { state, action in
        switch action.payload {
            case .present(let route):
                if !state.routeStack.contains(where: {
                    $0 == route
                }) {
                    state.routeStack.append(route)
                }
            case .pop:
                if state.routeStack.count > 1 {
                    let _ = state.routeStack.popLast()
                }
            case .popToRoute(let route):
                if let index = state.routeStack.lastIndex(of: route) {
                    state.routeStack.removeSubrange((index + 1)..<state.routeStack.count)
                }
            case .exit:
                state.routeStack = []
        }
    }),
    ReduceOn(LiveRoomNavigatorActions.setRootRoute, reduce: { state, action in
        if state.routeStack.count >= 1 {
            state.routeStack[0] = action.payload
        } else {
            state.routeStack.append(action.payload)
        }
    })
)


let liveRoomMenuReducer = Reducer<MenuState>(
    ReduceOn(LiveRoomViewActions.updateBottomMenus, reduce: { state, action in
        let creator = BottomPopupListViewDataHelper()
        state.menusButtons = creator.generateAudienceBottomMenuData()
    })
)
