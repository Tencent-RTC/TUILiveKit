//
//  LiveRoomViewState.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/24.
//

import Foundation

struct LiveRoomViewState {
    var rootViewState = ViewState()
    var menu = MenuState()
}

struct LiveRoomNavigationState {
    var routeStack: [LiveRouter.Route] = []
}

class PrintLiveRoomNavigateInterceptor: Interceptor {
    typealias State = LiveRoomNavigationState
    
    func actionDispatched(action: any Action, oldState: LiveRoomNavigationState, newState: LiveRoomNavigationState) {
        let name = String(describing: type(of: self))
        let actionName = String(describing: type(of: action))
        var actionLog = "\(name) - action dispatched: \(actionName)"
        print("action: \(actionLog), navigation stack: old: \(oldState.routeStack)")
        print("action: \(actionLog), navigation stack: new: \(newState.routeStack)")
    }
}
