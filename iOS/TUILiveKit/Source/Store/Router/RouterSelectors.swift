//
//  RouterSelectors.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import UIKit

class RouterSelectors: NSObject {
    static let routeStack = Selector(keyPath: \RouterState.routeStack)
    static let dismissEvent = Selector(keyPath: \RouterState.dismissEvent)
    static let currentRoute = Selector.with(routeStack) { stack in
        return stack.last
    }
}
