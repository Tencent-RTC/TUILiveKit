//
//  VRUserState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

import Foundation
import RTCRoomEngine
import ImSDK_Plus

struct VRUserState {
    var userList: [TUIUserInfo] = []
    var myFollowingUserList: Set<TUIUserInfo> = []
    var speakingUserList: Set<String> = []
    var linkStatus: LinkStatus = .none
    var fansCount: Int = 0
}

extension TUIUserInfo {
    convenience init(userFullInfo: V2TIMUserFullInfo) {
        self.init()
        userId = userFullInfo.userID ?? ""
        userName = userFullInfo.nickName ?? ""
        avatarUrl = userFullInfo.faceURL ?? ""
        userRole = TUIRole(rawValue: UInt(userFullInfo.role)) ?? .generalUser
    }
    
    convenience init(seatInfo: TUISeatInfo) {
        self.init()
        userId = seatInfo.userId ?? ""
        userName = seatInfo.userName ?? ""
        avatarUrl = seatInfo.avatarUrl ?? ""
    }
}
