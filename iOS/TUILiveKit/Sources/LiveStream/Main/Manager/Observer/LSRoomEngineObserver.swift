//
//  LSRoomEngineObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine

class LSRoomEngineObserver: NSObject {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
}

extension LSRoomEngineObserver: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        context?.roomManager.onLiveEnd()
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        context?.roomManager.onRoomUserCountChanged(roomId: roomId, userCount: userCount)
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        context?.userManager.onUserAudioStateChanged(userId: userId, hasAudio: hasAudio, reason: reason)
    }
    
    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        context?.userManager.onUserVideoStateChanged(userId: userId, streamType: streamType, hasVideo: hasVideo, reason: reason)
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
        context?.roomManager.onLiveEnd()
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        context?.userManager.onUserInfoChanged(userInfo: userInfo, modifyFlag: modifyFlag)
    }
}
