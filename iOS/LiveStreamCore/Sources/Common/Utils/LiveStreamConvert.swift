//
//  LiveStreamConvert.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/7.
//

import RTCRoomEngine

class LiveStreamConvert {
    static func convertToUserInfo(seatInfo: TUISeatInfo) -> TUIUserInfo {
        let userInfo = TUIUserInfo()
        userInfo.userId = seatInfo.userId ?? ""
        userInfo.userName = seatInfo.userName ?? ""
        userInfo.avatarUrl = seatInfo.avatarUrl ?? ""
        return userInfo
    }
    
    static func convertToUserInfo(request: TUIRequest) -> TUIUserInfo {
        let userInfo = TUIUserInfo()
        userInfo.userId = request.userId
        userInfo.userName = request.userName
        userInfo.avatarUrl = request.avatarUrl
        return userInfo
    }
    
    static func convertToCoHostUser(userInfo: TUIUserInfo, roomId: String, hasVideoStream: Bool, hasAudioStream: Bool) -> CoHostUser {
        let connectionUser = TUIConnectionUser()
        connectionUser.roomId = roomId
        connectionUser.userId = userInfo.userId
        connectionUser.userName = userInfo.userName
        connectionUser.avatarUrl = userInfo.avatarUrl
        let coHostUser = CoHostUser()
        coHostUser.connectionUser = connectionUser
        coHostUser.hasVideoStream = hasVideoStream
        coHostUser.hasAudioStream = hasAudioStream
        return coHostUser
    }
    
    static func convertToCoHostUser(connectionUser: TUIConnectionUser, hasVideoStream: Bool, hasAudioStream: Bool) -> CoHostUser {
        let coHostUser = CoHostUser()
        coHostUser.connectionUser = connectionUser
        coHostUser.hasVideoStream = hasVideoStream
        coHostUser.hasAudioStream = hasAudioStream
        return coHostUser
    }
    
    static func convertToUserInfoModifyFlag(flag: TUIUserInfoModifyFlag) -> UserInfoModifyFlag {
        var result: UserInfoModifyFlag = []
        if flag.contains(.userRole) {
            result.insert(.userRole)
        }
        if flag.contains(.nameCard) {
            result.insert(.nameCard)
        }
        return result
    }
}
