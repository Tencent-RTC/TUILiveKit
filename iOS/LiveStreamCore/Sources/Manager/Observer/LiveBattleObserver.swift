//
//  LiveBattleObserver.swift
//  LiveStreamCore
//
//  Created by adamsfliu on 2024/12/18.
//

import RTCRoomEngine

class LiveBattleObserver: NSObject, TUILiveBattleObserver {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
    
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleStarted(battleInfo: battleInfo)
            })
            context?.battleManager.onBattleStarted(battleInfo: battleInfo)
        }
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo, reason: TUIBattleStoppedReason) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleEnded(battleInfo: battleInfo)
            })
            context?.battleManager.onBattleEnded(battleInfo: battleInfo, reason: reason)
        }
    }
    
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onUserJoinBattle(battleId: battleId, battleUser: battleUser)
            })
            context?.battleManager.onUserJoinBattle(battleId: battleId, battleUser: battleUser)
        }
    }
    
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onUserExitBattle(battleId: battleId, battleUser: battleUser)
            })
            context?.battleManager.onUserExitBattle(battleId: battleId, battleUser: battleUser)
        }
    }
    
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser]) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleScoreChanged(battleId: battleId, battleUserList: battleUserList)
            })
            context?.battleManager.onBattleScoreChanged(battleId: battleId, battleUserList: battleUserList)
        }
    }
    
    func onBattleRequestReceived(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleRequestReceived(battleId: battleInfo.battleId, inviter: inviter, invitee: invitee)
            })
            context?.battleManager.onBattleRequestReceived(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
        }
    }
    
    func onBattleRequestCancelled(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleRequestCancelled(battleId: battleInfo.battleId, inviter: inviter, invitee: invitee)
            })
            context?.battleManager.onBattleRequestCancelled(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
        }
    }
    
    func onBattleRequestTimeout(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleRequestTimeout(battleId: battleInfo.battleId, inviter: inviter, invitee: invitee)
            })
            context?.battleManager.onBattleRequestTimeout(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
        }
    }
    
    func onBattleRequestAccept(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleRequestAccept(battleId: battleInfo.battleId, inviter: inviter, invitee: invitee)
            })
            context?.battleManager.onBattleRequestAccept(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
        }
    }
    
    func onBattleRequestReject(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        Task {
            await context?.observers.notifyBattleObservers(callback: { observer in
                observer.onBattleRequestReject(battleId: battleInfo.battleId, inviter: inviter, invitee: invitee)
            })
            context?.battleManager.onBattleRequestReject(battleInfo: battleInfo, inviter: inviter, invitee: invitee)
        }
    }
}
