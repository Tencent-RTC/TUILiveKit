//
//  LiveStreamObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/21.
//

import Foundation
import LiveStreamCore
import RTCRoomEngine

class AnchorConnectionObserver: NSObject {
    private let manager: AnchorManager
    
    init(manager: AnchorManager) {
        self.manager = manager
        super.init()
    }
}

extension AnchorConnectionObserver: ConnectionObserver {
    func onConnectedUsersUpdated(userList: [TUIUserInfo], joinList: [TUIUserInfo], leaveList: [TUIUserInfo]) {
    }
    
    func onUserConnectionRequest(inviterUser: TUIUserInfo) {
    }
    
    func onUserConnectionCancelled(inviterUser: TUIUserInfo) {
    }
    
    func onUserConnectionAccepted(userInfo: TUIUserInfo) {
    }
    
    func onUserConnectionRejected(userInfo: TUIUserInfo) {
        manager.onUserConnectionRejected(userId: userInfo.userId)
    }
    
    func onUserConnectionTimeout(userInfo: TUIUserInfo) {
        manager.onUserConnectionTimeout(userId: userInfo.userId)
    }
    
    func onUserConnectionTerminated() {
        manager.onKickedOffSeat()
    }
    
    func onUserConnectionExited(userInfo: TUIUserInfo) {
        
    }
    
    func onConnectedRoomsUpdated(hostUserList: [TUIConnectionUser]) {
        manager.onConnectionUserListChanged(list: hostUserList)
    }
    
    func onCrossRoomConnectionRequest(hostUser: TUIConnectionUser) {
        
    }
    
    func onCrossRoomConnectionCancelled(hostUser: TUIConnectionUser) {
        
    }
    
    func onCrossRoomConnectionAccepted(hostUser: TUIConnectionUser) {
        
    }
    
    func onCrossRoomConnectionRejected(hostUser: TUIConnectionUser) {
        
    }
    
    func onCrossRoomConnectionTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        
    }
    
    func onCrossRoomConnectionExited(hostUser: TUIConnectionUser) {
        
    }
    
    func onRoomDismissed(roomId: String) {
        
    }
}
