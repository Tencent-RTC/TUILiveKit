//
//  VoiceRoomViewStore.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/24.
//

import Combine

protocol VoiceRoomViewStore {
    var viewActionSubject: PassthroughSubject<any IdentifiableAction, Never> { get }
    
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<VoiceRoomViewState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<VoiceRoomViewState, Value>) -> Value
    func select<Value: Equatable>(_ selector: Selector<VoiceRoomNavigationState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<VoiceRoomNavigationState, Value>) -> Value
}

class VoiceRoomViewStoreProvider {
    let viewActionSubject = PassthroughSubject<any IdentifiableAction, Never>()
    
    private(set) lazy var store = Store(initialState: VoiceRoomViewState())
    private(set) lazy var navigator: Store<VoiceRoomNavigationState, Void> = Store(initialState: VoiceRoomNavigationState())
    
    init() {
        initializeStore()
    }
    
    private func initializeStore() {
        initializeNavigationStore()
        initializeViewStore()
    }
    
    private func initializeNavigationStore() {
        navigator.register(reducer: voiceRoomNavigationReducer)
    }
    
    private func initializeViewStore() {
        store.register(reducer: voiceRoomMenuReducer, for: \VoiceRoomViewState.menu)
    }
}

extension VoiceRoomViewStoreProvider: VoiceRoomViewStore {
    func dispatch(action: any Action) {
        guard let action = action as? IdentifiableAction else { return }
        if action.id.contains(VoiceRoomNavigatorActions.key) {
            navigator.dispatch(action: action)
        } else if action.id.contains(VoiceRoomViewActions.key) {
            store.dispatch(action: action)
        } else if action.id.hasPrefix(VoiceRoomViewResponseActions.key) {
            viewActionSubject.send(action)
        }
    }
    
    func select<Value>(_ selector: Selector<VoiceRoomViewState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return store.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<VoiceRoomViewState, Value>) -> Value {
        return store.selectCurrent(selector)
    }
    
    func select<Value>(_ selector: Selector<VoiceRoomNavigationState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
        return navigator.select(selector)
    }
    
    func selectCurrent<Value>(_ selector: Selector<VoiceRoomNavigationState, Value>) -> Value {
        return navigator.selectCurrent(selector)
    }
}
