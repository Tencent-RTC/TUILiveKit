//
//  BeautyStore.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/11/19.
//

import Combine

class BeautyStoreFactory {
    static var storeMap: [String : BeautyStoreProvider] = [:]
    
    static func getStore(roomId: String) -> BeautyStoreProvider? {
        if let beautyStore = storeMap[roomId] {
            return beautyStore
        }
        return nil
    }
    
    static func addStore(roomId: String, beautyStore: BeautyStoreProvider) {
        storeMap.updateValue(beautyStore, forKey: roomId)
    }
    
    static func removeStore(roomId: String) {
        storeMap.removeValue(forKey: roomId)
    }
    
    static func removeAllStore() {
        storeMap.removeAll()
    }
}

protocol BeautyStore {
    func dispatch(action: Action)
    func select<Value: Equatable>(_ selector: Selector<BeautyState, Value>) -> AnyPublisher<Value, Never>
    func selectCurrent<Value>(_ selector: Selector<BeautyState, Value>) -> Value
}
