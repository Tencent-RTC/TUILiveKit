//
//  AnchorUserState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine
import AtomicXCore

struct AnchorUserState {
    var userList: Set<TUIUserInfo> = []
    var speakingUserList: Set<String> = []
    var myFollowingUserList: Set<TUIUserInfo> = []
}
