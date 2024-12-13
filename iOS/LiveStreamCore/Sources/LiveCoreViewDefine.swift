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
    func updateCoGuestView(userInfo: TUIUserInfo, modifyFlag: UserInfoModifyFlag, coGuestView: UIView)
    func createCoHostView(coHostUser: CoHostUser) -> UIView?
    func updateCoHostView(coHostUser: CoHostUser, modifyFlag: UserInfoModifyFlag, coHostView: UIView)
}

@objc public protocol WaitingCoGuestViewDelegate {
    func waitingCoGuestView() -> UIView?
}

@objcMembers public class CoHostUser: NSObject {
    public var connectionUser: TUIConnectionUser = TUIConnectionUser()
    public var hasAudioStream: Bool = false
    public var hasVideoStream: Bool = true
}

@objc public class UserInfoModifyFlag: NSObject, OptionSet {
    public let rawValue: UInt

    @objc public static let none = UserInfoModifyFlag([])
    @objc public static let userRole = UserInfoModifyFlag(rawValue: 0x01 << 0)
    @objc public static let nameCard = UserInfoModifyFlag(rawValue: 0x01 << 1)
    @objc public static let hasVideoStream = UserInfoModifyFlag(rawValue: 0x01 << 2)
    @objc public static let hasAudioStream = UserInfoModifyFlag(rawValue: 0x01 << 3)

    required public init(rawValue: UInt) {
        self.rawValue = rawValue
    }
}

// MARK: ------------- Private Notification -------------
// MARK: - Room
public let LiveCoreViewOnEnterRoomNotifyName: Notification.Name = Notification.Name("__kLiveCoreView_NotifyName_Room_EnterRoom__")
public let LiveCoreViewOnExitRoomNotifyName: Notification.Name = Notification.Name("__kLiveCoreView_NotifyName_Room_ExitRoom__")
// MARK: - Video
// MARK: - Audio
