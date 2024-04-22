//
//  UserActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//


enum UserActions {
    static let key = "User.action"
    
    static let fetchUserInfo = ActionTemplate(id: key.appending(".onAnchorTakeSeat"), payloadType: NextActionTemplateParamTuple<String, User>.self)
    
    static let getSelfInfo = ActionTemplate(id: key.appending(".getSelfInfo"))
    // Async Action
    static let onUserAudioAvailable = ActionTemplate(id: key.appending(".onUserAudioAvailable"), payloadType: (String, Bool).self)
    static let onUserVoiceVolumeChanged = ActionTemplate(id: key.appending(".onUserVoiceVolumeChanged"), payloadType: Set<String>.self)
    static let onUserEnterRoom = ActionTemplate(id: key.appending(".onUserEnterRoom"), payloadType: User.self)
    static let onUserLeaveRoom = ActionTemplate(id: key.appending(".onUserLeaveRoom"), payloadType: User.self)
    static let fetchUserList = ActionTemplate(id: key.appending(".fetchUserList"))
    static let updateUserList = ActionTemplate(id: key.appending(".updateUserList"), payloadType: [User].self)
    static let updateReceivedGiftTotalPrice = ActionTemplate(id: key.appending(".updateReceivedGiftTotalPrice"), payloadType: Int.self)
    static let updateSendGiftUser = ActionTemplate(id: key.appending(".updateSendGiftUser"), payloadType: String.self)
    static let fetchRoomOwnerInfo = ActionTemplate(id: key.appending(".fetchRoomOwnerInfo"))
    static let updateRoomOwnerInfo = ActionTemplate(id: key.appending(".updateRoomOwnerInfo"), payloadType: User.self)
}
