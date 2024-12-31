//
//  BattleManager.swift
//  LiveStreamCore
//
//  Created by adamsfliu on 2024/12/19.
//

import RTCCommon
import RTCRoomEngine

class BattleManager {
    let observerState = ObservableState<BattleState>(initialState: BattleState())
    var battleState: BattleState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func requestBattle(config: TUIBattleConfig, userIdList: [String],
                       timeout: TimeInterval) async throws -> (String, [TUIBattleUser]) {
        do {
            let result = try await service.requestBattle(config: config, userIdList: userIdList, timeout: timeout)
            modifyBattleState(value: result.0.battleId, keyPath: \BattleState.battleId)
            
            let userIds = result.1.filter {
                $0.value.intValue == TUIBattleCode.success.rawValue
            }.map { $0.key }
            
            context?.coHostManager.coHostState.connectedUserList.filter { user in
                userIds.contains(user.userId)
            }.map { user in
                let battleUser = TUIBattleUser()
                battleUser.roomId = user.roomId
                battleUser.userId = user.userId
                battleUser.userName = user.userName
                battleUser.avatarUrl = user.avatarUrl
                return battleUser
            }.forEach({ battleUser in
                addSentBattleRequest(user: battleUser)
            })
            return (result.0.battleId, battleState.sentBattleRequestList)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","requestBattle:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
            LiveStreamLog.error("\(#file)","\(#line)","unknown error, description:[:\(error.localizedDescription)]")
            return ("", [])
        }
    }
    
    func cancelBattle(battleId: String, userIdList: [String]) async throws {
        let battleUserIdList = battleState.sentBattleRequestList.filter { battleUser in
            userIdList.contains(battleUser.userId)
        }.map { $0.userId }
        
        do {
            try await service.cancelBattle(battleId: battleId, userIdList: battleUserIdList)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","cancelBattle:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func acceptBattle(battleId: String) async throws {
        do {
            try await service.acceptBattle(battleId: battleId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","acceptBattle:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func rejectBattle(battleId: String) async throws {
        do {
            try await service.rejectBattle(battleId: battleId)
            modifyBattleState(value: [], keyPath: \BattleState.sentBattleRequestList)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","rejectBattle:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func exitBattle(battleId: String) async throws {
        do {
            try await service.exitBattle(battleId: battleId)
            modifyBattleState(value: [], keyPath: \BattleState.battledUsers, isPublished: true)
            modifyBattleState(value: "", keyPath: \BattleState.battleId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","rejectBattle:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
}

// MARK: - Observer
extension BattleManager {
    func onBattleStarted(battleInfo: TUIBattleInfo) {
        modifyBattleState(value: battleInfo.battleId, keyPath: \BattleState.battleId)
        modifyBattleState(value: true, keyPath: \BattleState.isBattleStart, isPublished: true)
        var battleUserList = battleInfo.inviteeList
        battleUserList.append(battleInfo.inviter)
        modifyBattleState(value: battleUserList, keyPath: \BattleState.battledUsers, isPublished: true)
    }
    
    func onBattleEnded(battleInfo: TUIBattleInfo, reason: TUIBattleStoppedReason) {
        modifyBattleState(value: "", keyPath: \BattleState.battleId)
        modifyBattleState(value: false, keyPath: \BattleState.isBattleStart, isPublished: true)
        modifyBattleState(value: [], keyPath: \BattleState.battledUsers, isPublished: true)
    }
    
    func onUserJoinBattle(battleId: String, battleUser: TUIBattleUser) {
        guard battleId == battleState.battleId else { return }
        addBattleUser(user: battleUser)
    }
    
    func onUserExitBattle(battleId: String, battleUser: TUIBattleUser) {
        guard battleId == battleState.battleId else { return }
        removeBattleUser(user: battleUser)
    }
    
    func onBattleScoreChanged(battleId: String, battleUserList: [TUIBattleUser]) {
        
    }
    
    func onBattleRequestReceived(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        modifyBattleState(value: battleInfo.battleId, keyPath: \BattleState.battleId)
    }
    
    func onBattleRequestCancelled(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        modifyBattleState(value: "", keyPath: \BattleState.battleId)
    }
    
    func onBattleRequestTimeout(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        modifyBattleState(value: "", keyPath: \BattleState.battleId)
    }
    
    func onBattleRequestAccept(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        modifyBattleState(value: battleInfo.battleId, keyPath: \BattleState.battleId)
    }
    
    func onBattleRequestReject(battleInfo: TUIBattleInfo, inviter: TUIBattleUser, invitee: TUIBattleUser) {
        modifyBattleState(value: "", keyPath: \BattleState.battleId)
    }
}

// MARK: - Private
extension BattleManager {
    private func modifyBattleState<T>(value: T, keyPath: WritableKeyPath<BattleState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { battleState in
            battleState[keyPath: keyPath] = value
        }
    }
    
    private func addBattleUser(user: TUIBattleUser) {
        observerState.update { battleState in
            battleState.battledUsers.append(user)
        }
    }
    
    private func removeBattleUser(user: TUIBattleUser) {
        observerState.update { battleState in
            if battleState.battledUsers.count == 2 { return }
            battleState.battledUsers.removeAll(where: {$0.userId == user.userId})
        }
    }
    
    private func addSentBattleRequest(user: TUIBattleUser) {
        observerState.update { battleState in
            battleState.sentBattleRequestList.append(user)
        }
    }
}
