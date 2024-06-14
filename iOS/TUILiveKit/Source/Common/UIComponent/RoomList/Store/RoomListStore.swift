//
//  RoomListStore.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Combine

protocol RoomListStoreProvider {
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<RoomListState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<RoomListState, Value>) -> Value
    func select<Value: Equatable>(_ selector: Selector<RoomListNavigationState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<RoomListNavigationState, Value>) -> Value
}

class RoomListStore {
    let service = RoomListService()
    private(set) lazy var store: Store<RoomListState, RoomListService> = Store(initialState: RoomListState(),environment: service)
    private(set) lazy var navigator: Store<RoomListNavigationState, Void> = Store(initialState: RoomListNavigationState())
    
    init() {
        initializeStore()
        initializeNavigationStore()
    }
    
    private func initializeStore() {
        store.register(reducer: roomListReducer)
        store.register(effects: RoomListEffects())
    }
    
    private func initializeNavigationStore() {
        navigator.register(reducer: roomListNavigationReducer)
    }
}

extension RoomListStore: RoomListStoreProvider {
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(RoomListNavigatorActions.key) {
            navigator.dispatch(action: action)
        } else if action.id.contains(RoomListActions.key) {
            store.dispatch(action: action)
        }
    }
    
    func select<Value>(_ selector: Selector<RoomListState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return store.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<RoomListState, Value>) -> Value {
        return store.selectCurrent(selector)
    }
    
    func select<Value>(_ selector: Selector<RoomListNavigationState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return navigator.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<RoomListNavigationState, Value>) -> Value {
        return navigator.selectCurrent(selector)
    }
}
