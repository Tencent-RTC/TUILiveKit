//
//  Nsobjc+Extension.swift
//  Pods
//
//  Created by ssc on 2025/7/3.
//

extension NSObject {
    @discardableResult
    func safeSetPropertyIfSetterExists<T>(_ value: T, forKey key: String) -> Bool {
        guard key.rangeOfCharacter(from: .whitespacesAndNewlines) == nil else {
            return false
        }

        let setter = "set\(key.prefix(1).capitalized)\(key.dropFirst()):"
        guard responds(to: Selector(setter)) else {
            return false
        }

        let mirror = Mirror(reflecting: value)
        if mirror.displayStyle == .optional && mirror.children.isEmpty {
            return false
        }

        setValue(value, forKey: key)
        return true
    }
}

