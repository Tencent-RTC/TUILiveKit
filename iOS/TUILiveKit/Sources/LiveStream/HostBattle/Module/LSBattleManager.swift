//
//  LSBattleManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

typealias BattleStateUpdateClosure = (inout LSBattleState) -> Void

class LSBattleManager {
    var observableState = ObservableState<LSBattleState>(initialState: LSBattleState())
    var state: LSBattleState {
        observableState.state
    }
    
    private let service: LSBattleService
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    private var timer: DispatchSourceTimer?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.battleService
        self.observableState = ObservableState(initialState: LSBattleState())
    }
    
    func subscribeState<Value>(_ stateSelector: StateSelector<LSBattleState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(stateSelector)
    }
    
    func resetState() {
        observableState.update { state in
            state = LSBattleState()
        }
    }
}

extension LSBattleManager {
    func requestBattle(userIds: [String], timeout: TimeInterval) {
        guard let context = context else { return }
        let isOnDisplayResult = state.isOnDisplayResult
        let selfUserId = context.userManager.userState.selfInfo.userId
        let isSelfInConnection = context.coHostManager.state.connectedUsers.contains(where: { $0.userId == selfUserId })
        guard !isOnDisplayResult && isSelfInConnection else {
            context.toastSubject.send(.battleDisableText)
            return
        }
        
        Task {
            let config = TUIBattleConfig()
            config.duration = battleDuration
            config.needResponse = true
            config.extensionInfo = ""
            do {
                let result = try await service.requestBattle(config: config, userIdList: userIds, timeout: timeout)
                let resultMap = result.resultMap
                let battleInfo = result.battleInfo
                observableState.update { state in
                    state.battleId = battleInfo.battleId
                    state.addSentBattleRequest(info: battleInfo, requestResultMap: resultMap)
                    state.isInWaiting = true
                }
            } catch let err {
                // throw error
            }
        }
    }
    
    func cancelBattleRequest() {
        Task {
            do {
                let userList = state.sentBattleRequest.inviteeIdList
                try await service.cancelBattleRequest(battleId: state.battleId, userIdList: userList)
                observableState.update { state in
                    state.isInWaiting = false
                    state.clearSentBattleReuqest()
                }
            } catch let err {
                
            }
        }
    }
    func acceptBattle() {
        Task {
            do {
                if let request = state.receivedBattleRequest {
                    try await service.acceptBattle(battleId: request.info.battleId)
                    observableState.update { state in
                        state.receivedBattleRequest = nil
                    }
                }
            } catch let err {
                
            }
        }
    }
    
    func rejectBattle() {
        Task {
            do {
                if let request = state.receivedBattleRequest {
                    try await service.rejectBattle(battleId: request.info.battleId)
                    observableState.update { state in
                        state.receivedBattleRequest = nil
                    }
                }
            } catch let err {
                
            }
        }
    }
    
    func exitBattle() {
        Task {
            do {
                try await service.exitBattle(battleId: state.battleId)
            } catch let err {
                
            }
        }
    }
    
    func isBattleDraw() -> Bool {
        let battleUsers = state.battleUsers
        guard let firstUser = battleUsers.first, let lastUser = battleUsers.last else { return false }
        return firstUser.ranking == lastUser.ranking
    }
    
    func resetBattleId() {
        observableState.update { state in
            state.battleId = ""
        }
    }
}

extension LSBattleManager {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        battleInfo.config.duration = battleInfo.config.duration + Double(battleInfo.startTime) - Date().timeIntervalSince1970
        observableState.update { state in
            state.battleId = battleInfo.battleId
            state.isBattleRunning = true
            state.isInWaiting = false
            state.isShowingStartView = true
            state.battleConfig = BattleConfig(battleInfo.config)
            state.durationCountDown = Int(battleInfo.config.duration)
        }
        startCountDown()
        
        var battleUsers = battleInfo.inviteeList.map { user in
            return BattleUser(user)
        }
        battleUsers.append(BattleUser(battleInfo.inviter))
        sortedBattleUsersByScore(battleUsers: battleUsers)
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo, reason: TUIBattleStoppedReason) {
        observableState.update { state in
            var battleUsers = battleInfo.inviteeList.map { user in
                return BattleUser(user)
            }
            battleUsers.append(BattleUser(battleInfo.inviter))
            state.battleUsers = battleUsers
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
            state.battleUsers.append(BattleUser(battleUser))
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
        sortedBattleUsersByScore(battleUsers: battleUserList.map { BattleUser($0) })
    }
    
    func onBattleRequestReceived(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.battleId = battleInfo.battleId
            state.receivedBattleRequest = (battleInfo, BattleUser(inviter), BattleUser(invitee))
        }
    }
    
    func onBattleRequestCancelled(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.receivedBattleRequest = nil
        }
        context?.toastSubject.send(.localizedReplace(.battleInviterCancelledText, replace: "\(inviter.userName)"))
    }
    
    func onBattleRequestTimeout(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            if state.receivedBattleRequest?.info.battleId == battleInfo.battleId {
                state.receivedBattleRequest = nil
            }
            // FIXME: krabyu  Here needs to fix when battleInfo.battleId is not empty
//            if state.sentBattleRequest.info?.battleId == battleInfo.battleId {
//                state.sentBattleRequest.inviteeIdList.removeAll { $0 == inviter.userId }
//                guard state.sentBattleRequest.inviteeIdList.isEmpty else { return }
//                state.isInWaiting = false
//            }
            state.sentBattleRequest.inviteeIdList.removeAll { $0 == invitee.userId }
            state.isInWaiting = false
            context?.toastSubject.send(.battleInvitationTimeoutText)
        }
    }
    
    func onBattleRequestAccept(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.sentBattleRequest.inviteeIdList.removeAll { $0 == invitee.userId }
            if state.sentBattleRequest.inviteeIdList.isEmpty {
                state.isInWaiting = false
            }
        }
    }
    
    func onBattleRequestReject(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        observableState.update { state in
            state.sentBattleRequest.inviteeIdList.removeAll { $0 == invitee.userId }
            if state.sentBattleRequest.inviteeIdList.isEmpty {
                state.isInWaiting = false
            }
        }
        let message = String.localizedReplace(.battleInvitationRejectText, replace: invitee.userName)
        context?.toastSubject.send(message)
    }
    
    func update(battleState: BattleStateUpdateClosure) {
        observableState.update(reduce: battleState)
    }
    
    private func sortedBattleUsersByScore(battleUsers: [BattleUser]) {
        guard !battleUsers.isEmpty else { return }
        observableState.update { state in
            state.battleUsers = battleUsers
                .sorted(by: { $0.score > $1.score })
                .enumerated()
                .map { (index, user) in
                    var updatedUser = user
                    if index > 0 && user.score == battleUsers[index - 1].score {
                        updatedUser.ranking = battleUsers[index - 1].ranking
                    } else {
                        updatedUser.ranking = index + 1
                    }
                    return updatedUser
                }
        }
    }
}

// MARK: Countdown Timer
extension LSBattleManager {
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
    static let battleDisableText = localized("live.error.battleDisable.unconnected")
    static let battleInviterCancelledText = localized("live.battle.invitation.cancelled.xxx")
    static let battleInvitationRejectText = localized("live.battle.invitation.reject.xxx")
    static let battleInvitationTimeoutText = localized("live.battle.invitation.timeout")
}
