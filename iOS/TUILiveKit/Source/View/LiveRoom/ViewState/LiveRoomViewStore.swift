//
//  LiveRoomViewStore.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//
import Combine

protocol LiveRoomViewStore {
    
    var navigateActionSubject: PassthroughSubject<LiveRouter.Action, Never> { get }
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<LiveRoomNavigationState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<LiveRoomNavigationState, Value>) -> Value
    func select<Value: Equatable>(_ selector: Selector<LiveRoomViewState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> Value
    
    func navigate(action: LiveRouter.Action)
}

class LiveRoomViewStoreProvider {
    private(set) lazy var store = Store(initialState: LiveRoomViewState())
    private(set) lazy var navigator: Store<LiveRoomNavigationState, Void> = Store(initialState: LiveRoomNavigationState())
    private(set) var navigateActionSubject = PassthroughSubject<LiveRouter.Action, Never>()
    
    init() {
        initializeStore()
    }
    
    private func initializeStore() {
        initializeNavigationStore()
    }
    
    private func initializeNavigationStore() {
        navigator.register(reducer: liveRoomNavigationReducer)
#if DEBUG
        navigator.register(interceptor: PrintLiveRoomNavigateInterceptor())
#endif
    }
    
    private func initializeViewStore() {
        store.register(reducer: liveRoomMenuReducer, for: \LiveRoomViewState.menu)
    }
}

extension LiveRoomViewStoreProvider: LiveRoomViewStore {
    
    func navigate(action: LiveRouter.Action) {
        navigator.dispatch(action: LiveRoomNavigatorActions.navigate(payload: action))
    }
    
    func select<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return store.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<LiveRoomViewState, Value>) -> Value {
        return store.selectCurrent(selector)
    }
    
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(LiveRoomNavigatorActions.key) {
            navigator.dispatch(action: action)
        } else if action.id.contains(LiveRoomViewActions.key) {
            store.dispatch(action: action)
        }
        
        if let action = action as? AnonymousAction<LiveRouter.Action> {
            navigateActionSubject.send(action.payload)
        }
    }
    
    func select<Value>(_ selector: Selector<LiveRoomNavigationState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return navigator.select(selector)
            .receive(on: RunLoop.main)
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<LiveRoomNavigationState, Value>) -> Value {
        return navigator.selectCurrent(selector)
    }
}

