//
//  RouterActions.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/14.
//

import Foundation

enum RouterActions {
    static let key = "Live.Router.Action"
    static let router = ActionTemplate(id: key.appending(".router"), payloadType: RouterAction.self)
    static let setRootRoute = ActionTemplate(id: key.appending(".setRootRoute"), payloadType: Route.self)
    static let clearDismissEvent = ActionTemplate(id: key.appending(".clearDismissEvent"))
}
