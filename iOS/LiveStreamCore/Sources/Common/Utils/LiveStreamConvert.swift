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
    
    static func convertToConnectionUser(userInfo: TUIUserInfo, roomId: String) -> TUIConnectionUser {
        let connectionUser = TUIConnectionUser()
        connectionUser.roomId = roomId
        connectionUser.userId = userInfo.userId
        connectionUser.userName = userInfo.userName
        connectionUser.avatarUrl = userInfo.avatarUrl
        return connectionUser
    }
}
