//
//  StoreFactory.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/5.
//

import Foundation

protocol StoreFactory {
    associatedtype T
    static var storeMap: [String : T] { get set }
    static func getStore(roomId: String) -> T
    static func removeStore(roomId: String)
    static func removeAllStore()
}

extension StoreFactory {
    static func removeStore(roomId: String) {
        storeMap.removeValue(forKey: roomId)
    }
    
    static func removeAllStore() {
        storeMap.removeAll()
    }
}
