//
//  LiveCoreViewDefine.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import Foundation
import RTCRoomEngine

@objc public enum LayoutMode: Int {
    case gridLayout
    case floatLayout
    case freeLayout
}

/**************************************** observer ****************************************/
@objc public protocol ConnectionObserver {
    func onConnectedUsersUpdated(userList: [TUIUserInfo], joinList: [TUIUserInfo], leaveList: [TUIUserInfo])
    func onUserConnectionRequest(inviterUser: TUIUserInfo)
    func onUserConnectionCancelled(inviterUser: TUIUserInfo)
    func onUserConnectionAccepted(userInfo: TUIUserInfo)
    func onUserConnectionRejected(userInfo: TUIUserInfo)
    func onUserConnectionTimeout(userInfo: TUIUserInfo)
    func onUserConnectionTerminated()
    func onUserConnectionExited(userInfo: TUIUserInfo)
    func onConnectedRoomsUpdated(hostUserList: [TUIConnectionUser])
    func onCrossRoomConnectionRequest(hostUser: TUIConnectionUser)
    func onCrossRoomConnectionCancelled(hostUser: TUIConnectionUser)
    func onCrossRoomConnectionAccepted(hostUser: TUIConnectionUser)
    func onCrossRoomConnectionRejected(hostUser: TUIConnectionUser)
    func onCrossRoomConnectionTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser)
    func onCrossRoomConnectionExited(hostUser: TUIConnectionUser)
    func onRoomDismissed(roomId: String)
}

@objc public protocol VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView?
    func updateCoGuestView(userInfo: TUIUserInfo, coGuestView: UIView)
    func createCoHostView(connectionUser: TUIConnectionUser) -> UIView?
    func updateCoHostView(connectionUser: TUIConnectionUser, coHostView: UIView)
}

@objc public protocol WaitingCoGuestViewDelegate {
    func waitingCoGuestView() -> UIView?
}
