//
//  LSCoHostObserver.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/23.
//

import Foundation
import RTCRoomEngine

class LSCoHostObserver: NSObject, TUILiveConnectionObserver {
    private(set) weak var context: LiveStreamManager.Context?
    init(context: LiveStreamManager.Context? = nil) {
        self.context = context
        super.init()
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser, inviteeList: [TUIConnectionUser], extensionInfo: String) {
        guard let context = self.context else { return }
        context.coHostManager.onConnectionRequestReceived(inviter: inviter, inviteeList: inviteeList, extensionInfo: extensionInfo)
    }
    
    func onConnectionRequestCancelled(inviter: TUIConnectionUser) {
    }
    
    func onConnectionRequestAccept(invitee: TUIConnectionUser) {
        guard let context = self.context else { return }
        context.coHostManager.onConnectionRequestAccept(invitee: invitee)
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        guard let context = self.context else { return }
        context.coHostManager.onConnectionRequestReject(invitee: invitee)
    }
    
    func onConnectionRequestTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        guard let context = self.context else { return }
        context.coHostManager.onConnectionRequestTimeout(inviter: inviter, invitee: invitee)
    }
    
    func onConnectionUserListChanged(connectedList: [TUIConnectionUser], joinedList: [TUIConnectionUser], leavedList: [TUIConnectionUser]) {
        guard let context = self.context else { return }
        context.coHostManager.onConnectionUserListChanged(list: connectedList)
    }
}
