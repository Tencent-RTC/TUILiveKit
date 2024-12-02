//
//  UserState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

struct UserState {
    var selfInfo: TUIUserInfo = TUIUserInfo()
    var hasAudioStreamUserList: Set<String> = []
    var hasVideoStreamUserList: Set<String> = []
}
