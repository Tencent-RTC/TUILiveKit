//
//  ConnectionService.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/5.
//

import RTCRoomEngine
import Combine

class ConnectionService: BaseServiceProtocol {
    
    var roomEngine: TUIRoomEngine
    
    var connectionManager: TUILiveConnectionManager {
        return roomEngine.getLiveConnectionManager()
    }
    
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    func getRecommendedList(cursor: String, count: Int = 20) -> AnyPublisher<(String, [TUILiveInfo]), InternalError> {
        return Future<(String,[TUILiveInfo]), InternalError> { [weak self] promise in
            guard let self = self else { return }
            guard let listManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                promise(.failure(InternalError(error:TUIError.failed, message: String.localized("live.error.failed"))))
                return
            }
            listManager.fetchLiveList(cursor: cursor, count: count) { responseCursor, responseLiveList in
                let liveList = responseLiveList.filter { info in
                    return LiveIdentityGenerator.shared.getIDType(info.roomInfo.roomId) == .live
                }
                promise(.success((responseCursor, liveList)))
            } onError: { error, message in
                promise(.failure(InternalError(error: error, message: message)))
            }
        }.eraseToAnyPublisher()
    }
    
    
    func requestConnection(roomIdList:[String], extensionInfo: String) -> AnyPublisher<[String:TUIConnectionCode], InternalError>  {
        return Future<[String:TUIConnectionCode], InternalError>  {[weak self] promise in
            guard let self = self else { return }
            connectionManager.requestConnection(roomIdList: roomIdList, timeout: 10, extensionInfo: extensionInfo) { result in
                var connectionResult:[String:TUIConnectionCode] = [:]
                result.forEach { (key: String, value: NSNumber) in
                    connectionResult[key] = TUIConnectionCode(rawValue: value.intValue) ?? .unknown
                }
                promise(.success(connectionResult))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func cancelRequest(roomIdList:[String]) -> AnyPublisher<Void, InternalError>{
        return Future<Void, InternalError>  {[weak self] promise in
            guard let self = self else { return }
            connectionManager.cancelConnectionRequest(roomIdList: roomIdList) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func accept(roomId:String) -> AnyPublisher<Void, InternalError>{
        return Future<Void, InternalError>  {[weak self] promise in
            guard let self = self else { return }
            connectionManager.acceptConnection(roomId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func reject(roomId:String) -> AnyPublisher<Void, InternalError>{
        return Future<Void, InternalError>  {[weak self] promise in
            guard let self = self else { return }
            connectionManager.rejectConnection(roomId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func disconnect() -> AnyPublisher<Void, InternalError>{
        return Future<Void, InternalError>  {[weak self] promise in
            guard let self = self else { return }
            connectionManager.disconnect {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
}
