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
            .flatMap { _ in
                environment.seatService.getSeatList()
                    .map { seatList in
                        if !seatList.isEmpty {
                            seatList.forEach { seatInfo in
                                if seatInfo.userId == environment.store?.selectCurrent(UserSelectors.currentUserId) {
                                    environment.store?.dispatch(action: MediaActions.operateMicrophone(payload: true))
                                }
                            }
                        }
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
                if let isOwner = environment.store?.selectCurrent(UserSelectors.isOwner), !isOwner {
                    environment.store?.dispatch(action: ViewActions.updateLinkStatus(payload: .applying))
                    environment.store?.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .takeSeatApplyingToast)))
                }
                return environment.seatService.takeSeat(index: action.payload, requestCallback: { [weak environment] request in
                    environment?.store?.dispatch(action: SeatActions.updateMySeatApplicationId(payload: request.requestId))
                })
                    .map { result in
                        handleTakeSeat(result: result, environment: environment)
                        var actions: [Action] = [
                            SeatActions.updateMySeatApplicationId(payload: ""),
                        ]
                        return actions
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
                        ViewActions.toastEvent(payload: ToastInfo(message: .operationSuccessful))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }

    let kickSeat = SeatEffect.dispatchingOne({ actions, environment in
        actions
            .wasCreated(from: SeatActions.kickSeat)
            .flatMap { action in
                environment.seatService.kickSeat(seat: action.payload)
                    .map { _ in
                        ViewActions.toastEvent(payload: ToastInfo(message: .operationSuccessful))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    })

    let leaveSeat = SeatEffect.dispatchingOne({ actions, environment in
        actions
            .wasCreated(from: SeatActions.leaveSeat)
            .flatMap { _ in
                environment.seatService.leaveSeat()
                    .map { _ in
                        ViewActions.toastEvent(payload: ToastInfo(message: .operationSuccessful))
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
            .flatMap { _ in
                environment.seatService.fetchSeatApplicationList()
                    .map { result in
                        SeatActions.onSeatApplicationListUpdate(payload: result)
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }

    let responseSeatApplication = SeatEffect.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: SeatActions.responseSeatApplication)
            .flatMap { action in
                environment.seatService.responseSeatApplication(isAgree: action.payload.0, requestId: action.payload.1)
                    .map {
                        [
                            SeatActions.handleApplicationSuccess(),
                            SeatActions.fetchSeatApplicationList(),
                        ]
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
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
                        SeatActions.handleApplicationSuccess()
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    static func handleTakeSeat(result: TakeSeatResult, environment: Environment) {
        guard let store = environment.store else { return }
        store.dispatch(action: SeatResponseActions.takeSeatResponse(payload: result))
        var linkStatus: LinkStatus = LinkStatus.none
        switch result {
            case .accepted:
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .takeSeatSuccess)))
                linkStatus = .linking
            case .rejected:
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .takeSeatApplicationRejected)))
            case .timeout:
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .takeSeatApplicationTimeout)))
            default:
                break
        }
        store.dispatch(action: ViewActions.updateLinkStatus(payload: linkStatus))
    }
}

fileprivate extension String {
    static let operationSuccessful = localized("live.error.success")
    static let takeSeatSuccess = localized("live.seat.takeSeatSuccess")
    static let takeSeatApplicationRejected = localized("live.seat.takeSeatApplicationRejected")
    static let takeSeatApplicationTimeout = localized("live.seat.takeSeatApplicationTimeout")
    static let takeSeatApplyingToast = localized("live.seat.takeSeatApplying")
}
