//
//  UserState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct UserState {
    public var selfInfo: TUIUserInfo = TUIUserInfo()
    var hasAudioStreamUserList: Set<String> = []
    var hasVideoStreamUserList: Set<String> = []
}
