//
//  LivePageState.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/7/30.
//

import Foundation
import LiveStreamCore
import Combine
import RTCRoomEngine

class LivePageState: ObservableObject {
    @Published private(set) var shouldPop: Bool = false
    
    let manager: SeatGridViewManager
    
    init(manager: SeatGridViewManager) {
        self.manager = manager
        manager.addObserver(self)
    }
    
    deinit {
        manager.removerObserver(self)
    }
    
}

extension LivePageState: SeatGridViewObserver {
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
    }
    
    func onSeatRequestReceived(type: LiveStreamCore.SGRequestType, userInfo: TUIUserInfo) {
    }
    
    func onSeatRequestCancelled(type: LiveStreamCore.SGRequestType, userInfo: TUIUserInfo) {
    }
    
    func onKickedOffSeat(userInfo: TUIUserInfo) {
    }
    
    func onUserAudioStateChanged(userInfo: TUIUserInfo, hasAudio: Bool, reason: TUIChangeReason) {
    }
    
    func onSeatViewClicked(seatView: UIView, seatInfo: TUISeatInfo) {
    }
    
    func onRoomDismissed(roomId: String) {
        shouldPop = true
    }
}
