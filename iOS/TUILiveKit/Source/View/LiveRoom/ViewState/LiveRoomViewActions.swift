//
//  LiveRoomViewActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/28.
//

import Foundation

enum LiveRoomViewActions {
    static let key = "LiveRoom.View.action"
    static let customEventKey = "LiveRoom.View.customEvent"
    
    static let updateBottomMenus = ActionTemplate(id: key.appending(".updateBottomMenu"),
                                                  payloadType: (LiveStore, RouterStore).self)
    static let like = ActionTemplate(id: customEventKey.appending(".like"))
}
