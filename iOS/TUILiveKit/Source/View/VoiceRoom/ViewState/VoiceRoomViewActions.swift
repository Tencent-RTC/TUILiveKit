//
//  VoiceRoomViewActions.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/6.
//

// Action


enum VoiceRoomNavigatorActions {
    static let key = "VoiceRoom.navigation.Action"
    static let navigatorTo = ActionTemplate(id: key.appending(".present"), payloadType: VoiceRoomNavigationState.Router.self)
}

enum VoiceRoomViewActions {
    static let key = "VoiceRoom.View.action"
    static let updateBottomMenus = ActionTemplate(id: key.appending(".updateBottomMenu"))
}

enum VoiceRoomViewResponseActions {
    static let key = "VoiceRoom.view.subject"
    static let like = ActionTemplate(id: key.appending(".like"))
}
