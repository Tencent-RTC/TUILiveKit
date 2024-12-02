//
//  LSRouterManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/20.
//

import RTCCommon
import Combine

class LSRouterManager {
    let observerState = ObservableState<LSRouterState>(initialState: LSRouterState())
    var routerState: LSRouterState {
        observerState.state
    }
    
    func subscribeRouterState<Value>(_ selector: StateSelector<LSRouterState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeRouterState() -> AnyPublisher<LSRouterState, Never> {
        return observerState.subscribe()
    }
}

extension LSRouterManager {
    func router(action: LSRouterAction) {
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
    
    func setRootRoute(route: LSRoute) {
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

extension LSRouterManager {
    func update(routerState: ((inout LSRouterState) -> Void)) {
        observerState.update(reduce: routerState)
    }
}
