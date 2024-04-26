//
//  RoomService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/8.
//

import RTCRoomEngine
import Combine

// TODO: - Dependency management, needs to consolidate RoomEngine instances.
class RoomService {
    private let engine = TUIRoomEngine.sharedInstance()
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func start(roomInfo: TUIRoomInfo) -> AnyPublisher<TUIRoomInfo, InternalError> {
        return createRoom(info: roomInfo)
            .flatMap({ [weak self] () -> AnyPublisher<TUIRoomInfo, InternalError> in
                guard let self = self else {
                    var error = InternalError(error: TUIError.failed, message: "RoomService instance is nil.")
                    error.actions.append(NavigatorActions.navigatorTo(payload: .exit))
                    return Fail(error: error).eraseToAnyPublisher()
                }
                return self.enterRoom(roomId: roomInfo.roomId)
            })
            .eraseToAnyPublisher()
    }
    
    func join(roomId: String) -> AnyPublisher<TUIRoomInfo, InternalError> {
        return enterRoom(roomId: roomId)
    }
    
    func leave() -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.exitRoom(syncWaiting: true) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func stop() -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.destroyRoom {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
}

extension RoomService {
    private func createRoom(info: TUIRoomInfo) -> AnyPublisher<Void, InternalError> {
        let future = Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.createRoom(info) {
                promise(.success(()))
            } onError: { err, message in
                var error = InternalError(error: err, message: message)
                error.actions.append(NavigatorActions.navigatorTo(payload: .exit))
                promise(.failure(error))
            }
        }
        return future.eraseToAnyPublisher()
    }
    
    private func enterRoom(roomId: String) -> AnyPublisher<TUIRoomInfo, InternalError> {
        let future = Future<TUIRoomInfo, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.enterRoom(roomId, roomType: .live) { roomInfo in
                guard let info = roomInfo else {
                    let error = InternalError(error: TUIError.roomIdInvalid, message: TUIError.roomIdInvalid.description)
                    promise(.failure(error))
                    return
                }
                promise(.success(info))
            } onError: { err, message in
                var error = InternalError(error: err, message: message)
                error.actions.append(NavigatorActions.navigatorTo(payload: .exit))
                promise(.failure(error))
            }
        }
        return future.eraseToAnyPublisher()

    }
}
