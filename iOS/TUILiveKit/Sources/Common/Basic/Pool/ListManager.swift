//
//  ListManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/23.
//

import Foundation

class ListManager<T> {
    private var storage: [T] = []
    private let maxLength: Int
    private(set) var totalCount = 0
    
    var count: Int {
        storage.count
    }
    
    init(maxLength: Int = 1000) {
        self.maxLength = maxLength
    }
    
    func removeAll() {
        storage.removeAll()
    }
    
    @discardableResult
    func popFirst() -> T? {
        if storage.isEmpty {
            return nil
        }
        return storage.removeFirst()
    }
    
    func insert(_ obj: T, at index: Int) {
        totalCount += 1
        if count + 1 > maxLength {
            storage.removeFirst()
        }
        storage.insert(obj, at: index)
    }
    
    func append(_ obj: T) {
        totalCount += 1
        if count + 1 > maxLength {
            storage.removeFirst()
        }
        storage.append(obj)
    }
    
    func append(contentsOf list: [T]) {
        totalCount += list.count
        if list.count > maxLength {
            storage.append(contentsOf: list)
            storage.removeFirst(list.count - maxLength)
            return
        }
        if count + list.count > maxLength {
            let removeCount = count + list.count - maxLength
            storage.removeFirst(removeCount)
        }
        storage.append(contentsOf: list)
    }
    
    @discardableResult
    func remove(at index: Int) -> T? {
        if index >= storage.count || index < 0 {
            return nil
        }
        return storage.remove(at: index)
    }
    
    func reverse(index: Int) -> T? {
        if index < 0 || index >= storage.count {
            return nil
        }
        let reverseIndex = storage.count - 1 - index
        return storage[reverseIndex]
    }
    
    subscript(index: Int) -> T? {
        if index >= storage.count || index < 0 {
            return nil
        }
        return storage[index]
    }
}

extension ListManager {
    func getList() -> [T] {
        storage
    }
    
    func callAsFunction() -> [T] {
        getList()
    }
}
