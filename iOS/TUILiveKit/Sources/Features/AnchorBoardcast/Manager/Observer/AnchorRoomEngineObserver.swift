//
//  AnchorRoomEngineObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine

class AnchorRoomEngineObserver: NSObject {
    private(set) weak var context: AnchorManager.Context?
    
    init(context: AnchorManager.Context) {
        self.context = context
        super.init()
    }
}

extension AnchorRoomEngineObserver: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        context?.roomManager.onLiveEnd(roomId: roomId, reason: reason)
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        context?.roomManager.onRoomUserCountChanged(roomId: roomId, userCount: userCount)
    }
    
    func onUserVoiceVolumeChanged(volumeMap: [String: NSNumber]) {
        context?.userManager.onUserVoiceVolumeChanged(volumeMap: volumeMap)
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        context?.userManager.onRemoteUserEnterRoom(roomId: roomId, userInfo: userInfo)
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        context?.userManager.onRemoteUserLeaveRoom(roomId: roomId, userInfo: userInfo)
    }
    
    func onKickedOffLine(message: String) {
        // TODO
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        context?.roomManager.onKickedOutOfRoom(roomId: roomId, reason: reason, message: message)
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        context?.userManager.onUserInfoChanged(userInfo: userInfo, modifyFlag: modifyFlag)
    }
    
    func OnSendMessageForUserDisableChanged(roomId: String, userId: String, isDisable muted: Bool) {
        context?.userManager.OnSendMessageForUserDisableChanged(roomId: roomId, userId: userId, isDisable: muted)
    }
}
