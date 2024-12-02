//
//  VRRouterManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/18.
//

import RTCCommon
import Combine

class VRRouterManager {
    let observerState = ObservableState<VRRouterState>(initialState: VRRouterState())
    var routerState: VRRouterState {
        observerState.state
    }
    
    func subscribeRouterState<Value>(_ selector: StateSelector<VRRouterState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeRouterState() -> AnyPublisher<VRRouterState, Never> {
        return observerState.subscribe()
    }
}

extension VRRouterManager {
    func router(action: VRRouterAction) {
        switch action {
        case .routeTo(let route):
            if let index = routerState.routeStack.lastIndex(of: route) {
                update { routerState in
                    routerState.routeStack.removeSubrange((index+1)..<routerState.routeStack.count)
                }
            }
        case .present(let route):
            if !routerState.routeStack.contains(where: { $0 == route}) {
                update { routerState in
                    routerState.routeStack.append(route)
                }
            }
        case .dismiss(let dimissType, let completion):
            if dimissType == .alert {
                if let currentRoute = routerState.routeStack.last, case .alert(_) = currentRoute {
                    handleDissmiss(completion: completion)
                }
            } else {
                handleDissmiss(completion: completion)
            }
        case .exit:
            update { routerState in
                routerState.routeStack = []
            }
        }
    }
    
    func setRootRoute(route: VRRoute) {
        if routerState.routeStack.count >= 1 {
            update { routerState in
                routerState.routeStack[0] = route
            }
        } else {
            update { routerState in
                routerState.routeStack.append(route)
            }
        }
    }
    
    func clearDismissEvent() {
        update { routerState in
            routerState.dismissEvent = nil
        }
    }
    
    private func handleDissmiss(completion: (() -> Void)? = nil) {
        update { routerState in
            routerState.dismissEvent = completion
        }
        if routerState.routeStack.count > 1 {
            update { routerState in
                let _ = routerState.routeStack.popLast()
            }
        }
    }
}

extension VRRouterManager {
    func update(routerState: ((inout VRRouterState) -> Void)) {
        observerState.update(reduce: routerState)
    }
}
