//
//  LiveStreamObserverList.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/7.
//

import Foundation

class LiveStreamObserverList {
    private(set) var observers: NSHashTable<ConnectionObserver> = NSHashTable(options: .weakMemory)
    
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
