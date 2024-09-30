 //
//  BattleService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import Combine
import RTCRoomEngine

class BattleService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    
    var battleManager: TUILiveBattleManager {
        roomEngine.getLiveBattleManager()
    }
    
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func requestBattle(config: TUIBattleConfig, userIdList: [String], timeout: TimeInterval)
    -> AnyPublisher<(TUIBattleInfo, [String: TUIBattleCode]), InternalError> {
        return Future<(TUIBattleInfo, [String: TUIBattleCode]), InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.battleManager.requestBattle(config: config, userIdList: userIdList, timeout: timeout) { battleInfo, resultMap in
                var battleResult: [String: TUIBattleCode] = [:]
                resultMap.forEach { (key: String, value: NSNumber) in
                    battleResult[key] = TUIBattleCode(rawValue: value.intValue) ?? .unknown
                }
                promise(.success((battleInfo, battleResult)))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func cancelBattleRequest(battleId: String, userIdList: [String]) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self]  promise in
            guard let self = self else { return }
            self.battleManager.cancelBattleRequest(battleId: battleId, userIdList: userIdList) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func acceptBattle(battleId: String) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.battleManager.acceptBattle(battleId: battleId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func rejectBattle(battleId: String) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.battleManager.rejectBattle(battleId: battleId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func exitBattle(battleId: String) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.battleManager.exitBattle(battleId: battleId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
}
