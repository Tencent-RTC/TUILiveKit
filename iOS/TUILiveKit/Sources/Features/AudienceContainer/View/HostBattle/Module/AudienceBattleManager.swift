//
//  AudienceBattleManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import LiveStreamCore

typealias AudienceBattleStateUpdateClosure = (inout AudienceBattleState) -> Void

class AudienceBattleManager {
    var observableState = ObservableState<AudienceBattleState>(initialState: AudienceBattleState())
    var state: AudienceBattleState {
        observableState.state
    }
    var coreBattleState: BattleState {
        context?.coreBattleState ?? BattleState()
    }
    
    private typealias Context = AudienceManager.Context
    private weak var context: Context?
    private var timer: DispatchSourceTimer?
    
    init(context: AudienceManager.Context) {
        self.context = context
        self.observableState = ObservableState(initialState: AudienceBattleState())
    }
    
    func subscribeState<Value>(_ stateSelector: StateSelector<AudienceBattleState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(stateSelector)
    }
    
    func resetState() {
        observableState.update { state in
            state = AudienceBattleState()
        }
    }
    
    func updateBattleUserRectFromIndex(rect: CGRect, index: Int) {
        observableState.update { state in
            state.battleUsers[index].rect = rect
        }
    }
}

extension AudienceBattleManager {
    func onRequestBattle(battleId: String, battleUserList: [TUIBattleUser]) {
        observableState.update { state in
            state.battleId = battleId
            state.isInWaiting = true
        }
    }
    
    func onCanceledBattle() {
        observableState.update { state in
            state.isInWaiting = false
            state.battleId = ""
        }
    }
    
    func onResponseBattle() {
        observableState.update { state in
            state.receivedBattleRequest = nil
        }
    }
    
    func onBattleExited() {
        observableState.update { state in
            state.battleId = ""
        }
    }
    
    func isBattleDraw() -> Bool {
        let battleUsers = state.battleUsers
        guard let firstUser = battleUsers.first, let lastUser = battleUsers.last else { return false }
        return firstUser.ranking == lastUser.ranking
    }
}

extension AudienceBattleManager {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        battleInfo.config.duration = battleInfo.config.duration + Double(battleInfo.startTime) - Date().timeIntervalSince1970
        observableState.update { state in
            state.battleId = battleInfo.battleId
            state.isBattleRunning = true
            state.isInWaiting = false
            state.isShowingStartView = true
            state.battleConfig = AudienceBattleConfig(battleInfo.config)
            state.durationCountDown = Int(battleInfo.config.duration)
        }
        startCountDown()
        
        var battleUsers = battleInfo.inviteeList.map { user in
            return AudienceBattleUser(user)
        }
        battleUsers.append(AudienceBattleUser(battleInfo.inviter))
        sortedBattleUsersByScore(battleUsers: battleUsers)
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo) {
        var battleUsers = battleInfo.inviteeList.map { user in
            return AudienceBattleUser(user)
        }
        battleUsers.append(AudienceBattleUser(battleInfo.inviter))
        sortedBattleUsersByScore(battleUsers: battleUsers)
        observableState.update { state in
            state.durationCountDown = 0
            state.isOnDisplayResult = true
            state.isBattleRunning = false
        }
        
        stopCountDown()
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak self] in
            guard let self = self else { return }
            self.observableState.update { state in
                state.isOnDisplayResult = false
            }
            self.resetState()
        }
    }
    
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser) {
        guard battleId == state.battleId else { return }
        observableState.update { state in
            state.battleUsers.append(AudienceBattleUser(battleUser))
        }
    }
    
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser) {
        observableState.update { state in
            if state.battleUsers.count == 2 { return }
            if state.battleUsers.contains(where: {$0.userId == battleUser.userId}) {
                state.battleUsers.removeAll(where: {$0.userId == battleUser.userId})
            }
        }
        sortedBattleUsersByScore(battleUsers: state.battleUsers)
    }
    
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser]) {
        sortedBattleUsersByScore(battleUsers: battleUserList.map { AudienceBattleUser($0) })
    }
    
    func onBattleRequestReceived(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.battleId = battleId
            state.receivedBattleRequest = (battleId, AudienceBattleUser(inviter))
        }
    }
    
    func onBattleRequestCancelled(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.receivedBattleRequest = nil
        }
        context?.toastSubject.send(.localizedReplace(.battleInviterCancelledText, replace: "\(inviter.userName)"))
    }
    
    func onBattleRequestTimeout(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            if state.receivedBattleRequest?.battleId == battleId {
                state.receivedBattleRequest = nil
            }
            // FIXME: krabyu  Here needs to fix when battleInfo.battleId is not empty
//            if state.sentBattleRequest.info?.battleId == battleInfo.battleId {
//                state.sentBattleRequest.inviteeIdList.removeAll { $0 == inviter.userId }
//                guard state.sentBattleRequest.inviteeIdList.isEmpty else { return }
//                state.isInWaiting = false
//            }
            state.isInWaiting = false
            context?.toastSubject.send(.battleInvitationTimeoutText)
        }
    }
    
    func onBattleRequestAccept(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            if coreBattleState.inviteeList.isEmpty {
                state.isInWaiting = false
            }
        }
    }
    
    func onBattleRequestReject(battleId: String, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            if coreBattleState.inviteeList.isEmpty {
                state.isInWaiting = false
            }
        }
        let message = String.localizedReplace(.battleInvitationRejectText, replace: invitee.userName)
        context?.toastSubject.send(message)
    }
    
    func update(battleState: AudienceBattleStateUpdateClosure) {
        observableState.update(reduce: battleState)
    }
    
    private func sortedBattleUsersByScore(battleUsers: [AudienceBattleUser]) {
        guard !battleUsers.isEmpty else { return }
        observableState.update { state in
            // 1. Sort with score
            // 2. If the second and subsequent shares are the same, the ranking is the same as the previous one, otherwise it is equal to the current number + 1
            var finalUsers: [AudienceBattleUser] = []
            let sorted = battleUsers.sorted(by: { $0.score > $1.score })
            for (index, user) in sorted.enumerated() {
                var updatedUser = user
                if index > 0 && user.score == sorted[index - 1].score {
                    updatedUser.ranking = sorted[index - 1].ranking
                } else {
                    updatedUser.ranking = index + 1
                }
                for battleUser in state.battleUsers {
                    if battleUser.userId == updatedUser.userId {
                        updatedUser.rect = battleUser.rect
                        break
                    }
                }
                finalUsers.append(updatedUser)
            }
            state.battleUsers = finalUsers
        }
    }
}

// MARK: Countdown Timer
extension AudienceBattleManager {
    private func startCountDown() {
        timer = DispatchSource.makeTimerSource(queue: DispatchQueue.main)
        timer?.schedule(deadline: .now() + 1, repeating: 1)
        timer?.setEventHandler { [weak self] in
            guard let self = self else { return }
            let t = self.state.durationCountDown
            if t > 0 {
                update { state in
                    state.durationCountDown = t - 1
                }
            } else {
                self.timer?.cancel()
            }
        }
        timer?.resume()
    }
    
    private func stopCountDown() {
        if let isCancelled = timer?.isCancelled, !isCancelled {
            timer?.cancel()
        }
    }
}

// MARK: Localized String
private extension String {
    static let battleInviterCancelledText = internalLocalized("xxx canceled battle, please try to initiate it again")
    static let battleInvitationRejectText = internalLocalized("xxx rejected battle")
    static let battleInvitationTimeoutText = internalLocalized("Battle request has been timeout")
}
