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

@objc public protocol BattleObserver {
    func onBattleStarted(battleInfo: TUIBattleInfo)
    func onBattleEnded(battleInfo: TUIBattleInfo)
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser)
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser)
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser])
    func onBattleRequestReceived(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser)
    func onBattleRequestCancelled(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser)
    func onBattleRequestTimeout(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser)
    func onBattleRequestAccept(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser)
    func onBattleRequestReject(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser)
}

@objc public protocol VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView?
    func updateCoGuestView(coGuestView: UIView, userInfo: TUIUserInfo, modifyFlag: UserInfoModifyFlag)
    func createCoHostView(coHostUser: CoHostUser) -> UIView?
    func updateCoHostView(coHostView: UIView, coHostUser: CoHostUser, modifyFlag: UserInfoModifyFlag)
    func createBattleView(battleUser: TUIBattleUser) -> UIView?
    func updateBattleView(battleView: UIView, battleUser: TUIBattleUser)
    func createBattleContainerView() -> UIView?
    func updateBattleContainerView(battleContainerView: UIView, userInfos: [BattleUserViewModel])
}

@objc public protocol WaitingCoGuestViewDelegate {
    func waitingCoGuestView() -> UIView?
}

@objcMembers public class CoHostUser: NSObject {
    public var connectionUser: TUIConnectionUser = TUIConnectionUser()
    public var hasAudioStream: Bool = false
    public var hasVideoStream: Bool = true
}

@objcMembers public class BattleUserViewModel: NSObject {
    public var battleUser: TUIBattleUser = TUIBattleUser()
    public var rect: CGRect = .zero
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

public typealias TUIBattleRequestBlock = (String, [TUIBattleUser]) -> Void

// MARK: ------------- Private Notification -------------
// MARK: - Room
public let LiveCoreViewOnEnterRoomNotifyName: Notification.Name = Notification.Name("__kLiveCoreView_NotifyName_Room_EnterRoom__")
public let LiveCoreViewOnExitRoomNotifyName: Notification.Name = Notification.Name("__kLiveCoreView_NotifyName_Room_ExitRoom__")
// MARK: - Video
// MARK: - Audio
