//
//  AudienceBattleManagerObserver.swift
//  TUILiveKit
//
//  Created by gg on 2024/11/23.
//

import Foundation
import RTCRoomEngine
import AtomicXCore

class AudienceBattleManagerObserver: NSObject {
    private weak var manager: AudienceBattleManager?
    init(battleManager: AudienceBattleManager) {
        self.manager = battleManager
        super.init()
    }
}

extension AudienceBattleManagerObserver: BattleObserver {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        manager?.onBattleStarted(battleInfo: battleInfo)
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo) {
        manager?.onBattleEnded(battleInfo: battleInfo)
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
    
    func onBattleRequestReceived(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestReceived(battleId: battleId, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestCancelled(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestCancelled(battleId: battleId, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestTimeout(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestTimeout(battleId: battleId, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestAccept(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestAccept(battleId: battleId, inviter: inviter, invitee: invitee)
    }
    
    func onBattleRequestReject(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        manager?.onBattleRequestReject(battleId: battleId, inviter: inviter, invitee: invitee)
    }
}
