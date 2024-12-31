//
//  LiveStreamObserverList.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/7.
//

import Foundation

class LiveStreamObserverList {
    private(set) var observers: NSHashTable<ConnectionObserver> = NSHashTable(options: .weakMemory)
    private(set) var battleObservers: NSHashTable<BattleObserver> = NSHashTable(options: .weakMemory)
}

// MARK: - ConnectionObserver
extension LiveStreamObserverList {
    func addObserver(_ observer: ConnectionObserver) {
        if !observers.contains(observer) {
            observers.add(observer)
        }
    }
    
    func removeObserver(_ observer: ConnectionObserver) {
        observers.remove(observer)
    }
    
    @MainActor
    func notifyObservers(callback: (_ observer: ConnectionObserver) -> Void) {
        for observer in observers.allObjects {
            callback(observer)
        }
    }
}

// MARK: - BattleObserver
extension LiveStreamObserverList {
    func addBattleObserver(_ observer: BattleObserver) {
        if !battleObservers.contains(observer) {
            battleObservers.add(observer)
        }
    }
    
    func removeBattleObserver(_ observer: BattleObserver) {
        battleObservers.remove(observer)
    }
    
    @MainActor
    func notifyBattleObservers(callback: (_ observer: BattleObserver) -> Void) {
        for observer in battleObservers.allObjects {
            callback(observer)
        }
    }
}
