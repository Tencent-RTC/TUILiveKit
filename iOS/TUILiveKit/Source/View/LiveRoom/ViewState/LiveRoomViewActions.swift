//
//  LiveRoomViewActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//

import Foundation

enum LiveRoomNavigatorActions {
    static let key = "LiveRoom.navigation.Action"
    static let navigate = ActionTemplate(id: key.appending(".present"), payloadType: LiveRouter.Action.self)
    static let setRootRoute = ActionTemplate(id: key.appending(".present"), payloadType: LiveRouter.Route.self)
}

enum LiveRoomViewActions {
    static let key = "LiveRoom.View.action"
    static let subjectKey = "LiveRoom.View.subject"
    static let customEventKey = "LiveRoom.View.customEvent"
    
    static let updateBottomMenus = ActionTemplate(id: key.appending(".updateBottomMenu"))
    
    
    static let like = ActionTemplate(id: customEventKey.appending(".like"))
}
