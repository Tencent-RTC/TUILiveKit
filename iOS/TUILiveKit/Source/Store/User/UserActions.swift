//
//  UserActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import RTCRoomEngine

enum UserActions {
    static let key = "User.action"
    // Action
    static let fetchUserInfo = ActionTemplate(id: key.appending(".onAnchorTakeSeat"), payloadType: NextActionTemplateParamTuple<String, User>.self)
    static let getSelfInfo = ActionTemplate(id: key.appending(".getSelfInfo"))
    
    static let startPlayRemoteVideo = ActionTemplate(id: key.appending(".startPlayRemoteVideo"), payloadType: (String, TUIVideoStreamType).self)
    static let stopPlayRemoteVideo = ActionTemplate(id: key.appending(".stopPlayRemoteVideo"), payloadType: (String, TUIVideoStreamType).self)
    static let fetchUserList = ActionTemplate(id: key.appending(".fetchUserList"))
    static let updateUserList = ActionTemplate(id: key.appending(".updateUserList"), payloadType: [User].self)
    static let follow = ActionTemplate(id: key.appending(".follow"), payloadType: String.self)
    static let unfollow = ActionTemplate(id: key.appending(".unfollow"), payloadType: String.self)
    static let checkFollowType = ActionTemplate(id: key.appending(".checkFollowType"), payloadType: String.self)
    static let fetchFollowersCount = ActionTemplate(id: key.appending(".fetchFollowersCount"), payloadType: String.self)
    static let updateFollowersCount = ActionTemplate(id: key.appending(".updateFollowersCount"), payloadType: Int.self)
    static let updateRemoteVideoView = ActionTemplate(
        id: key.appending(".updateRemoteVideoView"),
        payloadType: (String, TUIVideoStreamType, RenderView).self)
    
    // Passive Action
    static let onUserAudioAvailable = ActionTemplate(id: key.appending(".onUserAudioAvailable"), payloadType: (String, Bool).self)
    static let onUserVideoAvailable = ActionTemplate(id: key.appending(".onUserVideoAvailable"), payloadType: (String, Bool).self)
    static let onUserVoiceVolumeChanged = ActionTemplate(id: key.appending(".onUserVoiceVolumeChanged"), payloadType: Set<String>.self)
    static let onUserEnterRoom = ActionTemplate(id: key.appending(".onUserEnterRoom"), payloadType: User.self)
    static let onUserLeaveRoom = ActionTemplate(id: key.appending(".onUserLeaveRoom"), payloadType: User.self)
    static let onUserInMyFollowingList = ActionTemplate(id: key.appending(".onUserInMyFollowingList"), payloadType: (User, Bool).self)
    static let onPlayingRemoteVideoView = ActionTemplate(id: key.appending(".onPlayingRemoteVideoView"), payloadType: (String).self)
    static let onLoadingRemoteVideoView = ActionTemplate(id: key.appending(".onLoadingRemoteVideoView"), payloadType: (String).self)
}

// MARK: - Subject action, only event, no reduce.
enum UserResponseActions {
    static let key = "User.response"
    static let like = ActionTemplate(id: key.appending(".like"))
}
