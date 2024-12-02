//
//  BeautyStoreProvider.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/19.
//

import Combine
import RTCRoomEngine

class BeautyStoreProvider {
    let service: BeautyService
    private(set) lazy var store: Store<BeautyState, BeautyService> = Store(initialState: BeautyState(), environment: self.service)
    
    init() {
        service = BeautyService(roomEngine: TUIRoomEngine.sharedInstance())
        initializeStore()
    }
    
    deinit {
        unInitializeStore()
        print("deinit \(type(of: self))")
    }
    
    private func initializeStore() {
        store.register(reducer: beautyReducer)
        store.register(effects: BeautyEffects())
    }
    
    private func unInitializeStore() {
        store.unregister(reducer: beautyReducer)
        store.unregisterEffects(withId: BeautyEffects.id)
    }
}

extension BeautyStoreProvider: BeautyStore {
    func dispatch(action: Action) {
        store.dispatch(action: action)
    }
    
    func select<Value>(_ selector: Selector<BeautyState, Value>) -> AnyPublisher<Value, Never> where Value : Equatable {
       return store.select(selector)
            .removeDuplicates()
            .eraseToAnyPublisher()
    }
    
    func selectCurrent<Value>(_ selector: Selector<BeautyState, Value>) -> Value {
       return store.selectCurrent(selector)
    }
}
