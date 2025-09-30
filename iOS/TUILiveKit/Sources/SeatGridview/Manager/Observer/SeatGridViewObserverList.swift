//
//  SGSeatGridObserverList.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/25.
//

import Foundation

class SGSeatGridObserverList {
    private(set) var observers: NSHashTable<SeatGridViewObserver> = NSHashTable(options: .weakMemory)
    
    func addObserver(_ observer: SeatGridViewObserver) {
        if !observers.contains(observer) {
            observers.add(observer)
        }
    }
    
    func removeObserver(_ observer: SeatGridViewObserver) {
        observers.remove(observer)
    }
    
    @MainActor
    func notifyObservers(callback: (_ observer: SeatGridViewObserver) -> Void) {
        for observer in observers.allObjects {
            callback(observer)
        }
    }
}
