//
//  LiveListStore.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Combine
import RTCRoomEngine

protocol LiveListStore {
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<LiveListState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<LiveListState, Value>) -> Value
    func select<Value: Equatable>(_ selector: Selector<LiveListNavigationState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<LiveListNavigationState, Value>) -> Value
}

class LiveListStoreProvider {
    let service = LiveListService(roomEngine: TUIRoomEngine.sharedInstance())
    private(set) lazy var store: Store<LiveListState, LiveListService> = Store(initialState: LiveListState(),environment: service)
    private(set) lazy var navigator: Store<LiveListNavigationState, Void> = Store(initialState: LiveListNavigationState())
    
    init() {
        initializeStore()
        initializeNavigationStore()
    }
    
    private func initializeStore() {
        store.register(reducer: liveListReducer)
        store.register(effects: LiveListEffects())
    }
    
    private func initializeNavigationStore() {
        navigator.register(reducer: liveListNavigationReducer)
    }
}

extension LiveListStoreProvider: LiveListStore {
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(LiveListNavigatorActions.key) {
            navigator.dispatch(action: action)
        } else if action.id.contains(LiveListActions.key) {
            store.dispatch(action: action)
        }
    }
    
    func select<Value>(_ selector: Selector<LiveListState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return store.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<LiveListState, Value>) -> Value {
        return store.selectCurrent(selector)
    }
    
    func select<Value>(_ selector: Selector<LiveListNavigationState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return navigator.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<LiveListNavigationState, Value>) -> Value {
        return navigator.selectCurrent(selector)
    }
}
