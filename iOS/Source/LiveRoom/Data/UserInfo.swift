//
//  UserInfo.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import RTCRoomEngine
import TUICore

class UserInfo: Hashable {
    var userId: String = ""
    var name: Observable<String> = Observable("")
    var avatarUrl: Observable<String> = Observable("")
    var role: Observable<RoleType> = Observable(.none)
    let status: Observable<UserInteractionStatus> = Observable(.none)
    var audioInfo: AudioInfo = AudioInfo()
    var videoInfo: VideoInfo = VideoInfo()
    let beautyInfo: BeautyInfo = BeautyInfo()
    var requestId:String = ""
   
    func update(_ userInfo:TUIUserInfo) {
        userId = userInfo.userId
        name.value = userInfo.userName
        avatarUrl.value = userInfo.avatarUrl
        if userId != TUILogin.getUserID() ?? "" {
            role.value = (userInfo.userRole == .roomOwner) ? .anchor : .audience
            videoInfo.isCameraOpened.value = userInfo.hasVideoStream
            audioInfo.muteAudio.value = !userInfo.hasAudioStream
        }
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }

    static func == (lhs: UserInfo, rhs: UserInfo) -> Bool {
        return lhs.userId == rhs.userId
    }
}
