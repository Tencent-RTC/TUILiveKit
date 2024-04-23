//
//  SeatEffects.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import Combine

class SeatEffects: Effects {
    typealias Environment = ServiceCenter
    private typealias SeatEffect = Effect<Environment>
    
    let fetchSeatList = SeatEffect.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: SeatActions.fetchSeatList)
            .flatMap { action in
                environment.seatService.getSeatList()
                    .map { seatList in
                        let result = seatList.map { SeatInfo(info: $0) }
                        return SeatActions.seatListChanged(payload: result)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let takeSeat = SeatEffect.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: SeatActions.takeSeat)
            .flatMap { action in
                environment.seatService.takeSeat(index: action.payload)
                    .map { result in
                        return SeatEffects.handleMySeatApplication(result: result)
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let lockSeat = SeatEffect.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: SeatActions.lockSeat)
            .flatMap { action in
                environment.seatService.lockSeat(index: action.payload.0, lockMode: action.payload.1)
                    .map { _ in
                        return ViewActions.toast(payload: ToastInfo(message: .operationSuccessful))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let kickSeat = SeatEffect.dispatchingOne ({ actions, environment in
        actions
            .wasCreated(from: SeatActions.kickSeat)
            .flatMap { action in
                environment.seatService.kickSeat(seat: action.payload)
                    .map { _ in
                        return ViewActions.toast(payload: ToastInfo(message: .operationSuccessful))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    })
    
    let leaveSeat = SeatEffect.dispatchingOne ({ actions, environment in
        actions
            .wasCreated(from: SeatActions.leaveSeat)
            .flatMap { action in
                environment.seatService.leaveSeat()
                    .map { _ in
                        return ViewActions.toast(payload: ToastInfo(message: .operationSuccessful))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    })
    
    let fetchSeatApplication = SeatEffect.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: SeatActions.fetchSeatApplicationList)
            .flatMap { action in
                environment.seatService.fetchSeatApplicationList()
                    .map { result in
                        
                        return SeatActions.onSeatApplicationListUpdate(payload: result)
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let responseSeatApplication = SeatEffect.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: SeatActions.responseSeatApplication)
            .flatMap { action in
                environment.seatService.responseSeatApplication(isAgree: action.payload.0 , requestId: action.payload.1)
                    .map {
                        return SeatActions.handleApplicationSuccess()
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let cancelSeatApplication = SeatEffect.dispatchingOne {
        actions, environment in
        actions
            .wasCreated(from: SeatActions.cancelApplication)
            .flatMap { action in
                environment.seatService.cancelRequest(requestId: action.payload)
                    .map {
                        return SeatActions.handleApplicationSuccess()
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
}

extension SeatEffects {
    static func handleMySeatApplication(result: TakeSeatResult) -> [Action] {
        var actions = [SeatActions.updateMySeatApplicationId(payload: "")]
        switch result {
            case .accepted(let requestId, let userId):
                break
            case .rejected(let requestId, let userId, let message):
                break
            case .cancel(let requestId, let userId):
                break
            case .timeout(let requestId, let userId):
                break
        }
        return actions
    }
}

fileprivate extension String {
    static let operationSuccessful = localized("live.error.success")
}
