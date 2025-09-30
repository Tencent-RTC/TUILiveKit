//
//  SeatGridViewDefine.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/22.
//

import Foundation
import RTCRoomEngine

// MARK: - Enum define.
@objc public enum SGRequestType: Int {
    case applyToTakeSeat
    case inviteToTakeSeat
}

@objc public enum SGLayoutMode: Int {
    case focus
    case grid
    case vertical
    case free
}

@objc public enum SGSeatViewLayoutRowAlignment: Int {
    case spaceAround
    case spaceBetween
    case spaceEvenly
    case start
    case end
    case center
}

// MARK: - Class define.
@objcMembers
public class SGSeatViewLayoutConfig: NSObject {
    let rowConfigs: [SGSeatViewLayoutRowConfig]
    let rowSpacing: CGFloat
    
    public init(rowConfigs: [SGSeatViewLayoutRowConfig] = [SGSeatViewLayoutRowConfig](repeating: SGSeatViewLayoutRowConfig(), count: 2),
         rowSpacing: CGFloat = 22) {
        self.rowConfigs = rowConfigs
        self.rowSpacing = rowSpacing
    }
}

@objcMembers
public class SGSeatViewLayoutRowConfig: NSObject {
    let count: Int
    let seatSpacing: CGFloat
    let seatSize: CGSize
    let alignment: SGSeatViewLayoutRowAlignment
    
    public init(count: Int = 5,
         seatSpacing: CGFloat = 20.0,
         seatSize: CGSize = CGSize(width: 50, height: 88),
         alignment: SGSeatViewLayoutRowAlignment = .center) {
        self.count = count
        self.seatSpacing = seatSpacing
        self.seatSize = seatSize
        self.alignment = alignment
    }
}

// MARK: - Closure define.
public typealias SGOnSuccess = () -> Void
public typealias SGOnError = (_ code: Int, _ message: String) -> Void
public typealias SGOnRoomSuccess = (_ roomInfo: TUIRoomInfo) -> Void
public typealias SGOnRequestAccepted = (_ userInfo: TUIUserInfo) -> Void
public typealias SGOnRequestRejected = (_ userInfo: TUIUserInfo) -> Void
public typealias SGOnRequestCancelled = (_ userInfo: TUIUserInfo) -> Void
public typealias SGOnRequestTimeout = (_ userInfo: TUIUserInfo) -> Void
public typealias SGOnRequestError = (_ userInfo: TUIUserInfo, _ code: Int, _ message: String) -> Void

// MARK: - Constants define.
public let kSGDefaultMaxSeatCount = 10
public let kSGDefaultTimeout = 30

// MARK: - Delegate define.
@objc public protocol SGSeatViewDelegate {
    func seatGridView(_ view: SeatGridView, createSeatView seatInfo: TUISeatInfo) -> UIView?
    func seatGridView(_ view: SeatGridView, updateSeatView seatInfo: TUISeatInfo, seatView: UIView)
    func seatGridView(_ view: SeatGridView, updateUserVolume volume: Int, seatView: UIView)
}

// MARK: - Observer define.
@objc public protocol SeatGridViewObserver {
    func onRoomDismissed(roomId: String)
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String)
    func onSeatRequestReceived(type: SGRequestType, userInfo: TUIUserInfo)
    func onSeatRequestCancelled(type: SGRequestType, userInfo: TUIUserInfo)
    func onKickedOffSeat(userInfo: TUIUserInfo)
    func onSeatViewClicked(seatView: UIView, seatInfo: TUISeatInfo)
}

extension SeatGridViewObserver {
    func onRoomDismissed(roomId: String) {}
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {}
    func onSeatRequestReceived(type: SGRequestType, userInfo: TUIUserInfo) {}
    func onSeatRequestCancelled(type: SGRequestType, userInfo: TUIUserInfo) {}
    func onKickedOffSeat(userInfo: TUIUserInfo) {}
    func onSeatViewClicked(seatView: UIView, seatInfo: TUISeatInfo) {}
}

// MARK: ------------- Private Notification -------------
// MARK: - Room
public let SeatGridViewOnEnterRoomNotifyName: Notification.Name = Notification.Name("__kSeatGridView_NotifyName_Room_EnterRoom__")
public let SeatGridViewOnExitRoomNotifyName: Notification.Name = Notification.Name("__kSeatGridView_NotifyName_Room_ExitRoom__")
// MARK: - Audio
