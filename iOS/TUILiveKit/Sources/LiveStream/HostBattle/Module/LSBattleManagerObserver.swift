//
//  LSBattleManagerObserver.swift
//  TUILiveKit
//
//  Created by gg on 2024/11/23.
//

import Foundation
import RTCRoomEngine

class LSBattleManagerObserver: NSObject {
    private(set) weak var context: LiveStreamManager.Context?
    private weak var manager: LSBattleManager?
    init(context: LiveStreamManager.Context) {
        self.context = context
        manager = context.battleManager
        super.init()
    }
}

extension LSBattleManagerObserver: TUILiveBattleObserver {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        manager?.onBattleStarted(battleInfo: battleInfo)
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo, reason: TUIBattleStoppedReason) {
        manager?.onBattleEnded(battleInfo: battleInfo, reason: reason)
    }
    
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser) {
        manager?.onUserJoinBattle(battleId: battleId, battleUser: battleUser)
    }
    
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser) {
        manager?.onUserExitBattle(battleId: battleId, battleUser: battleUser)
    }
    
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser]) {
        manager?.onBattleScoreChanged(battleId: battleId, battleUserList: battleUserList)
    }
    
    func onBattleRequestReceived(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestReceived(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestCancelled(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestCancelled(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestTimeout(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestTimeout(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestAccept(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestAccept(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestReject(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestReject(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
    }
}
