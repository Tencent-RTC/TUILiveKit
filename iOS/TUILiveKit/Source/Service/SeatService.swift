//
//  SeatService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

import RTCRoomEngine
import Combine

enum TakeSeatResult {
    case accepted(_ requestId: String, _ operateUserId: String)
    case rejected(_ requestId: String, _ operateUserId: String, _ message: String)
    case timeout(_ requestId: String, _ operateUserId: String)
    case cancel(_ requestId: String, _ operateUserId: String)
}

private let kRandomSeatIndex = -1
private let kTakeSeatTimeoutValue = 60

typealias RequestClosure = (TUIRequest) -> Void

class SeatService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine?
    required init(roomEngine: TUIRoomEngine?) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func getSeatList() -> AnyPublisher<[TUISeatInfo], InternalError> {
        return Future<[TUISeatInfo], InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getSeatList { seatList in
                promise(.success(seatList))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func takeSeat(index: Int?, requestCallback:@escaping RequestClosure) -> AnyPublisher<TakeSeatResult, InternalError> {
        return Future<TakeSeatResult, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            let request = roomEngine.takeSeat(index ?? kRandomSeatIndex , timeout: 60) { requestId, operateUserId in
                let result = TakeSeatResult.accepted(requestId, operateUserId)
                promise(.success(result))
            } onRejected: {  requestId, operateUserId, message in
                let result = TakeSeatResult.rejected(requestId, operateUserId, message)
                promise(.success(result))
            } onCancelled: {  requestId, operateUserId in
                let result = TakeSeatResult.cancel(requestId, operateUserId)
                promise(.success(result))
            } onTimeout: {  requestId, operateUserId in
                let result = TakeSeatResult.timeout(requestId, operateUserId)
                promise(.success(result))
            } onError: {  requestId, operateUserId, err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
            requestCallback(request)
        }
        .eraseToAnyPublisher()
    }
    
    func leaveSeat() -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.leaveSeat {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func switchSeat(index: Int) -> AnyPublisher<Void, InternalError> {
        // TODO: adamsfliu need implement
        return Future<Void, InternalError> { promise in
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func lockSeat(index: Int, lockMode: TUISeatLockParams) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.lockSeatByAdmin(index, lockMode: lockMode) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func kickSeat(seat: SeatInfo) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.kickUserOffSeatByAdmin(seat.index, userId: seat.userId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func fetchSeatApplicationList() -> AnyPublisher <[SeatApplication], InternalError> {
        return Future { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getSeatApplicationList { requests in
                let result = requests.map { request in
                    let seatApplication = SeatApplication(request: request)
                    return seatApplication
                }
                promise(.success(result))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func responseSeatApplication(isAgree: Bool, requestId: String) -> AnyPublisher <Void, InternalError> {
        return Future { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.responseRemoteRequest(requestId, agree: isAgree) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func cancelRequest(requestId: String) -> AnyPublisher<Void, InternalError> {
        return Future { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.cancelRequest(requestId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
}
