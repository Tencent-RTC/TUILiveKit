//
//  LSBattleService.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/19.
//

import Foundation
import RTCRoomEngine

protocol LSBattleService {
    func requestBattle(config: TUIBattleConfig, userIdList: [String], timeout: TimeInterval) async throws
    -> (battleInfo: TUIBattleInfo, resultMap: [String: TUIBattleCode])
    func cancelBattleRequest(battleId: String, userIdList: [String]) async throws
    func acceptBattle(battleId: String) async throws
    func rejectBattle(battleId: String) async throws
    func exitBattle(battleId: String) async throws
}

class EngineBattleService: LSBattleService {
    
    private let battleManager: TUILiveBattleManager = TUIRoomEngine.sharedInstance().getLiveBattleManager()
    
    func requestBattle(config: TUIBattleConfig, userIdList: [String], timeout: TimeInterval) async throws -> (battleInfo: TUIBattleInfo, resultMap: [String : TUIBattleCode]) {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            battleManager.requestBattle(config: config, userIdList: userIdList, timeout: timeout) { battleInfo, resultMap in
                var battleResult: [String: TUIBattleCode] = [:]
                resultMap.forEach { (key: String, value: NSNumber) in
                    battleResult[key] = TUIBattleCode(rawValue: value.intValue) ?? .unknown
                }
                continuation.resume(returning: (battleInfo, battleResult))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func cancelBattleRequest(battleId: String, userIdList: [String]) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            battleManager.cancelBattleRequest(battleId: battleId, userIdList: userIdList) {
                continuation.resume()
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func acceptBattle(battleId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            battleManager.acceptBattle(battleId: battleId) {
                continuation.resume()
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func rejectBattle(battleId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            battleManager.rejectBattle(battleId: battleId) {
                continuation.resume()
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func exitBattle(battleId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            battleManager.exitBattle(battleId: battleId) {
                continuation.resume()
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
}
