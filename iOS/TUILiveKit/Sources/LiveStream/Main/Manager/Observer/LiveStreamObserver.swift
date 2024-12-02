//
//  LiveStreamObserver.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/21.
//

import Foundation
import LiveStreamCore
import RTCRoomEngine

class LiveStreamObserver: NSObject {
    private let manager: LiveStreamManager
    
    init(manager: LiveStreamManager) {
        self.manager = manager
        super.init()
    }
}

extension LiveStreamObserver: ConnectionObserver {
    func onConnectedUsersUpdated(userList: [TUIUserInfo], joinList: [TUIUserInfo], leaveList: [TUIUserInfo]) {
        manager.onSeatListChanged(userList: userList, joinList: joinList, leaveList: leaveList)
    }
    
    func onUserConnectionRequest(inviterUser: TUIUserInfo) {
        manager.onRequestReceived(inviter: inviterUser)
    }
    
    func onUserConnectionCancelled(inviterUser: TUIUserInfo) {
        manager.onRequestCancelled(inviter: inviterUser)
    }
    
    func onUserConnectionAccepted(userInfo: TUIUserInfo) {
        manager.onUserConnectionAccepted(userId: userInfo.userId)
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
        manager.coHostManager.onConnectionUserListChanged(list: hostUserList)
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
