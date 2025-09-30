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

    static func convertToLiveInfo(roomInfo: TUIRoomInfo) -> TUILiveInfo {
        let liveInfo = TUILiveInfo()
        liveInfo.roomId = roomInfo.roomId
        liveInfo.name = roomInfo.name
        liveInfo.isSeatEnabled = roomInfo.isSeatEnabled
        liveInfo.seatMode = roomInfo.seatMode
        liveInfo.maxSeatCount = roomInfo.maxSeatCount
        liveInfo.isMessageDisableForAllUser = roomInfo.isMessageDisableForAllUser
        liveInfo.keepOwnerOnSeat = roomInfo.keepOwnerOnSeat
        liveInfo.safeSetPropertyIfSetterExists(roomInfo.ownerId, forKey: "ownerId")
        liveInfo.safeSetPropertyIfSetterExists(roomInfo.ownerName, forKey: "ownerName")
        liveInfo.safeSetPropertyIfSetterExists(roomInfo.ownerAvatarUrl, forKey: "ownerAvatarUrl")
        liveInfo.safeSetPropertyIfSetterExists(roomInfo.createTime, forKey: "createTime")
        return liveInfo
    }

    static func convertToRoomInfo(liveInfo: TUILiveInfo) -> TUIRoomInfo {
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = liveInfo.roomId
        roomInfo.name = liveInfo.name
        roomInfo.isSeatEnabled = liveInfo.isSeatEnabled
        roomInfo.seatMode = liveInfo.seatMode
        roomInfo.maxSeatCount = liveInfo.maxSeatCount
        roomInfo.isMessageDisableForAllUser = liveInfo.isMessageDisableForAllUser
        roomInfo.keepOwnerOnSeat = liveInfo.keepOwnerOnSeat
        roomInfo.safeSetPropertyIfSetterExists(liveInfo.ownerId, forKey: "ownerId")
        roomInfo.safeSetPropertyIfSetterExists(liveInfo.ownerName, forKey: "ownerName")
        roomInfo.safeSetPropertyIfSetterExists(liveInfo.ownerAvatarUrl, forKey: "ownerAvatarUrl")
        roomInfo.safeSetPropertyIfSetterExists(liveInfo.createTime, forKey: "createTime")
        return roomInfo
    }
}
