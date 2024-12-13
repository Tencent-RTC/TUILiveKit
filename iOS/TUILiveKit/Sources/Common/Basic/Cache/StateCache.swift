//
//  StateCache.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/5.
//

import Foundation
import RTCCommon

enum CacheError: Error {
    case notCreate
    case typeMismatch
}

final class StateCache {
    
    private var storage: [String: Any] = [:]
    private var removeAction: [String: (() -> ())] = [:]
    
    static let shared = StateCache()
    init() {}
    
    func contains(key: String) -> Bool {
        return storage.keys.contains(key)
    }
    
    @discardableResult
    func setObject<T>(key: String, obj: T, force: Bool = false) -> Bool {
        if force {
            storage[key] = obj
            return true
        }
        if storage.keys.contains(key) {
            return false
        }
        storage[key] = obj
        return true
    }
    
    func getObject<T>(key: String) throws -> T {
        if storage.keys.contains(key) {
            if let obj = storage[key] as? T {
                return obj
            }
            throw CacheError.typeMismatch
        } else {
            throw CacheError.notCreate
        }
    }
    
    func removeObject(key: String) {
        if removeAction.keys.contains(key) {
            if let action = removeAction[key] {
                action()
            }
            removeAction.removeValue(forKey: key)
        }
        storage.removeValue(forKey: key)
    }
    
    func clear() {
        for action in removeAction.values {
            action()
        }
        removeAction.removeAll()
        storage.removeAll()
    }
    
    subscript<T>(key: String) -> T? {
        get {
            do {
                let res: T = try getObject(key: key)
                return res
            } catch let err {
                debugPrint(err.localizedDescription)
                return nil
            }
        }
    }
    
    func subscribeToObjectRemoval(key: String, action: @escaping (() -> ())) {
        removeAction[key] = action
    }
    
    func unsubscribeToObjectRemoval(key: String) {
        removeAction.removeValue(forKey: key)
    }
}
