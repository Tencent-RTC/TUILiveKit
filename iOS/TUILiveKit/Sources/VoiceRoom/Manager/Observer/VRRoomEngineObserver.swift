//
//  VRRoomEngineObserver.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/15.
//

import Foundation
import RTCRoomEngine

protocol VRRoomEngineObserverRoomInterface {
    func onRoomNameChanged(roomId: String, roomName: String)
    func onRoomDismissed(roomId: String)
    func onRoomUserCountChanged(roomId: String, userCount: Int)
    func onKickedOffLine(message: String)
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String)
}

protocol VRRoomEngineObserverUserInterface {
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo)
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo)
}

class VRRoomEngineObserver: NSObject {
    private(set) weak var context: VoiceRoomManager.Context?
    init(context: VoiceRoomManager.Context) {
        self.context = context
        super.init()
    }
}

extension VRRoomEngineObserver {
    private var roomManager: VRRoomManager? {
        context?.roomManager
    }
    private var userManager: VRUserManager? {
        context?.userManager
    }
    private var seatManager: VRSeatManager? {
        context?.seatManager
    }
}

// MARK: - TUIRoomObserver
extension VRRoomEngineObserver: TUIRoomObserver {
    func onError(error errorCode: TUIError, message: String) {
        if errorCode == .success {
            return
        }
        let error = InternalError(code: errorCode.rawValue, message: message)
        context?.toastSubject.send(error.localizedMessage)
    }
    
    func onRoomNameChanged(roomId: String, roomName: String) {
        roomManager?.onRoomNameChanged(roomId: roomId, roomName: roomName)
    }
    
    func onRoomDismissed(roomId: String) {
        roomManager?.onRoomDismissed(roomId: roomId)
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        userManager?.onRemoteUserEnterRoom(roomId: roomId, userInfo: userInfo)
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        userManager?.onRemoteUserLeaveRoom(roomId: roomId, userInfo: userInfo)
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        roomManager?.onRoomUserCountChanged(roomId: roomId, userCount: userCount)
    }
    
    func onKickedOffLine(message: String) {
        roomManager?.onKickedOffLine(message: message)
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        roomManager?.onKickedOutOfRoom(roomId: roomId, reason: reason, message: message)
    }
}
