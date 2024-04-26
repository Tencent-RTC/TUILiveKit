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

class SeatService {
    
    @WeakLazyInjected var store: VoiceRoomStoreProvider?
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    private var engine: TUIRoomEngine {
        return TUIRoomEngine.sharedInstance()
    }
    
    func getSeatList() -> AnyPublisher<[TUISeatInfo], InternalError> {
        return Future<[TUISeatInfo], InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.getSeatList { [weak self] seatList in
                guard let self = self, let store = self.store else { return }
                if !seatList.isEmpty {
                    seatList.forEach { seatInfo in
                        if seatInfo.userId == store.selectCurrent(UserSelectors.currentUserId) {
                            store.dispatch(action: MediaActions.operateMicrophone(payload: true))
                        }
                    }
                }
                promise(.success(seatList))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func takeSeat(index: Int?) -> AnyPublisher<TakeSeatResult, InternalError> {
        return Future<TakeSeatResult, InternalError> { [weak self] promise in
            guard let self = self else { return }
            let request = self.engine.takeSeat(index ?? kRandomSeatIndex , timeout: 60) { [weak self] requestId, operateUserId in
                guard let self = self, let store = self.store else { return }
                store.dispatch(action: ViewActions.toast(payload: ToastInfo(message: .takeSeatSuccess)))
                let result = TakeSeatResult.accepted(requestId, operateUserId)
                promise(.success(result))
            } onRejected: { [weak self] requestId, operateUserId, message in
                guard let self = self, let store = self.store else { return }
                store.dispatch(action: ViewActions.toast(payload: ToastInfo(message: .takeSeatApplicationRejected)))
                let result = TakeSeatResult.rejected(requestId, operateUserId, message)
                promise(.success(result))
            } onCancelled: { requestId, operateUserId in
                let result = TakeSeatResult.cancel(requestId, operateUserId)
                promise(.success(result))
            } onTimeout: { [weak self] requestId, operateUserId in
                guard let self = self, let store = self.store else { return }
                store.dispatch(action: ViewActions.toast(payload: ToastInfo(message: .takeSeatApplicationTimeout)))
                let result = TakeSeatResult.timeout(requestId, operateUserId)
                promise(.success(result))
            } onError: { requestId, operateUserId, err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
            // this action should not dispatch here, but other idea?
            store?.dispatch(action: SeatActions.updateMySeatApplicationId(payload: request.requestId))
        }
        .eraseToAnyPublisher()
    }
    
    func leaveSeat() -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.leaveSeat {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
    
    func switchSeat(index: Int) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { promise in
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func lockSeat(index: Int, lockMode: TUISeatLockParams) -> AnyPublisher<Void, InternalError> {
        return Future<Void, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.lockSeatByAdmin(index, lockMode: lockMode) {
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
            guard let self = self else { return }
            self.engine.kickUserOffSeatByAdmin(seat.index, userId: seat.userId) {
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
            guard let self = self else { return }
            self.engine.getSeatApplicationList { requests in
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
            guard let self = self else { return }
            self.engine.responseRemoteRequest(requestId, agree: isAgree) {
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
            guard let self = self else { return }
            self.engine.cancelRequest(requestId) {
                promise(.success(()))
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }
        .eraseToAnyPublisher()
    }
}

fileprivate extension String {
    static let takeSeatSuccess = localized("live.seat.takeSeatSuccess")
    static let takeSeatApplicationRejected = localized("live.seat.takeSeatApplicationRejected")
    static let takeSeatApplicationTimeout = localized("live.seat.takeSeatApplicationTimeout")
}
