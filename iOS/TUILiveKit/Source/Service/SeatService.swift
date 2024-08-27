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
private let kTimeoutValue: TimeInterval = 60

typealias RequestClosure = (TUIRequest) -> Void

class SeatService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func getSeatList() -> AnyPublisher<[TUISeatInfo], InternalError> {
        return Future<[TUISeatInfo], InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getSeatList")
            roomEngine.getSeatList { seatList in
                LiveKitLog.info("\(#file)", "\(#line)", "getSeatList[onSuccess]")
                promise(.success(seatList))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getSeatList[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func takeSeat(index: Int?, requestCallback:@escaping RequestClosure) -> AnyPublisher<TakeSeatResult, InternalError> {
        return Future<TakeSeatResult, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "takeSeat[index:\(String(describing: index))]")
            let request = roomEngine.takeSeat(index ?? kRandomSeatIndex , timeout: kTimeoutValue) { requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)", "takeSeat[onAccepted:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.accepted(requestId, operateUserId)
                promise(.success(result))
            } onRejected: {  requestId, operateUserId, message in
                LiveKitLog.info("\(#file)", "\(#line)", 
                                "takeSeat[onRejected:[requestId:\(requestId),operateUserId:\(operateUserId),message:\(message)]]")
                let result = TakeSeatResult.rejected(requestId, operateUserId, message)
                promise(.success(result))
            } onCancelled: {  requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)", 
                                "takeSeat[onCancelled:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.cancel(requestId, operateUserId)
                promise(.success(result))
            } onTimeout: {  requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)", 
                                "takeSeat[onTimeout:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.timeout(requestId, operateUserId)
                promise(.success(result))
            } onError: {  requestId, operateUserId, err, message in
                LiveKitLog.error("\(#file)", "\(#line)",
                                "takeSeat[onError:[requestId:\(requestId),operateUserId:\(operateUserId),error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
            requestCallback(request)
        }
        .eraseToAnyPublisher()
    }
    
    func leaveSeat() -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "leaveSeat")
            roomEngine.leaveSeat {
                LiveKitLog.info("\(#file)", "\(#line)", "leaveSeat[onSuccess]")
                promise(.success(()))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "leaveSeat[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func takeUserOnSeatByAdmin(seatIndex: Int,
                               userId: String,
                               requestCallback:@escaping RequestClosure) -> AnyPublisher<TakeSeatResult, InternalError> {
        return Future<TakeSeatResult, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "takeUserOnSeatByAdmin:[seatIndex:\(seatIndex),userId:\(userId)]")
            let request = roomEngine.takeUserOnSeatByAdmin(seatIndex, userId: userId, timeout: kTimeoutValue) { requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)", 
                                "takeUserOnSeatByAdmin[onAccepted:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.accepted(requestId, operateUserId)
                promise(.success(result))
            } onRejected: { requestId, operateUserId, message in
                LiveKitLog.info("\(#file)", "\(#line)",
                                "takeUserOnSeatByAdmin[onRejected:[requestId:\(requestId),operateUserId:\(operateUserId),message:\(message)]]")
                let result = TakeSeatResult.rejected(requestId, operateUserId, message)
                promise(.success(result))
            } onCancelled: { requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)",
                                "takeUserOnSeatByAdmin[onCancelled:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.cancel(requestId, operateUserId)
                promise(.success(result))
            } onTimeout: { requestId, operateUserId in
                LiveKitLog.info("\(#file)", "\(#line)", 
                                "takeUserOnSeatByAdmin[onTimeout:[requestId:\(requestId),operateUserId:\(operateUserId)]]")
                let result = TakeSeatResult.timeout(requestId, operateUserId)
                promise(.success(result))
            } onError: { requestId, operateUserId, err, message in
                let log = "takeUserOnSeatByAdmin[onError:[requestId:\(requestId),operateUserId:\(operateUserId),error:\(err) message:\(message)]]"
                LiveKitLog.error("\(#file)", "\(#line)",log)
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
            requestCallback(request)
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
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "lockSeatByAdmin:[index:\(index),lockMode:\(lockMode)]")
            roomEngine.lockSeatByAdmin(index, lockMode: lockMode) {
                LiveKitLog.info("\(#file)", "\(#line)", "lockSeatByAdmin:[onSuccess]")
                promise(.success(()))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "lockSeatByAdmin[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func kickSeat(seat: SeatInfo) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "kickUserOffSeatByAdmin:[index:\(seat.index),userId:\(seat.userId)]")
            roomEngine.kickUserOffSeatByAdmin(seat.index, userId: seat.userId) {
                LiveKitLog.info("\(#file)", "\(#line)", "kickUserOffSeatByAdmin:[onSuccess]")
                promise(.success(()))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "kickUserOffSeatByAdmin[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func fetchSeatApplicationList() -> AnyPublisher <[SeatApplication], InternalError> {
        return Future { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getSeatApplicationList")
            roomEngine.getSeatApplicationList { requests in
                LiveKitLog.info("\(#file)", "\(#line)", "getSeatApplicationList:[onSuccess]")
                let result = requests.map { request in
                    let seatApplication = SeatApplication(request: request)
                    return seatApplication
                }
                promise(.success(result))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getSeatApplicationList[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func responseRemoteRequest(isAgree: Bool, requestId: String) -> AnyPublisher <Void, InternalError> {
        return Future { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "responseRemoteRequest[isAgree:\(isAgree),requestId:\(requestId)]")
            roomEngine.responseRemoteRequest(requestId, agree: isAgree) {
                LiveKitLog.info("\(#file)", "\(#line)", "responseRemoteRequest:[onSuccess]")
                promise(.success(()))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "responseRemoteRequest[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func cancelRequest(requestId: String) -> AnyPublisher<Void, InternalError> {
        return Future { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "cancelRequest[requestId:\(requestId)]")
            roomEngine.cancelRequest(requestId) {
                LiveKitLog.info("\(#file)", "\(#line)", "cancelRequest:[onSuccess]")
                promise(.success(()))
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "cancelRequest[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
}
