//
//  RouterReducer.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import Foundation

let routerReducer = Reducer<RouterState>(
    ReduceOn(RouterActions.router, reduce: { state, action in
        switch action.payload {
        case .routeTo(let route):
            if let index = state.routeStack.lastIndex(of: route) {
                state.routeStack.removeSubrange((index + 1)..<state.routeStack.count)
            }
        case .present(let route):
            if !state.routeStack.contains(where: {
                $0 == route
            }) {
                state.routeStack.append(route)
            }
        case .dismiss(let animated, let completion):
            state.dismissEvent = (animated, completion)
            if state.routeStack.count > 1 {
                let _ = state.routeStack.popLast()
            }
        case .exit:
            state.routeStack = []
        }
    }),
    ReduceOn(RouterActions.setRootRoute, reduce: { state, action in
        if state.routeStack.count >= 1 {
            state.routeStack[0] = action.payload
        } else {
            state.routeStack.append(action.payload)
        }
    }),
    ReduceOn(RouterActions.clearDismissEvent, reduce: { state, action in
        state.dismissEvent = nil
    })
)
