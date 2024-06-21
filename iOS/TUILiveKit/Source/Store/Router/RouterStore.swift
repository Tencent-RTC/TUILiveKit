//
//  RouterStore.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import Combine

protocol RouterStore {
    func select<Value: Equatable>(_ selector: Selector<RouterState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<RouterState, Value>) -> Value
    func router(action: RouterAction)
    func dispatch(action: any Action)
}

class RouterStoreProvider {
    private(set) lazy var routerStore = Store(initialState: RouterState())
    init() {
        initializeRouterStore()
    }
    
    private func initializeRouterStore() {
        routerStore.register(reducer: routerReducer)
#if DEBUG
        routerStore.register(interceptor: PrintRouterStateInterceptor())
#endif
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}

extension RouterStoreProvider: RouterStore {
    
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(RouterActions.key) {
            routerStore.dispatch(action: action)
        }
    }
   
    func router(action: RouterAction) {
        dispatch(action: RouterActions.router(payload: action))
    }
    
    func select<Value>(_ selector: Selector<RouterState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return routerStore.select(selector)
            .receive(on: RunLoop.main)
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<RouterState, Value>) -> Value {
        return routerStore.selectCurrent(selector)
    }
}

