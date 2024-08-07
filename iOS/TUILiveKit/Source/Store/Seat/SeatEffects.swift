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
                    let seatMode = environment.store?.selectCurrent(RoomSelectors.getRoomSeatMode) ?? .applyToTake
                    if seatMode == .applyToTake {
                        environment.store?.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .takeSeatApplyingToast)))
                    }
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
    
    let inviteSeat = SeatEffect.dispatchingMultiple({ actions, environment in
        actions
            .wasCreated(from: SeatActions.inviteSeat)
            .flatMap { action in
                environment.seatService.takeUserOnSeatByAdmin(seatIndex: action.payload.0, userId: action.payload.1.userId) { request in
                    request.userName = action.payload.1.name
                    request.avatarUrl = action.payload.1.avatarUrl
                    let seatInvitation = SeatInvitation(request: request)
                    environment.store?.dispatch(action: SeatActions.addSeatInvitation(payload: seatInvitation))
                }
                .map { result in
                    handleInviteTakeSeat(result: result, environment: environment)
                }
                .catch { error -> Just<[Action]> in
                    let action = environment.errorService.convert(error: error)
                    return Just([action])
                }
                
            }
            .eraseToAnyPublisher()
    })
    
    let cancelInviteSeat = SeatEffect.dispatchingOne({ actions, environment in
        actions.wasCreated(from: SeatActions.cancelInviteSeat)
            .flatMap { action in
                environment.seatService.cancelRequest(requestId: action.payload.id)
                    .map {
                        SeatActions.removeSeatInvitation(payload: action.payload)
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    })
    
    let responseSeatInvitation = SeatEffect.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: SeatActions.responseSeatInvitation)
            .flatMap { action in
                environment.seatService.responseRemoteRequest(isAgree: action.payload.0, requestId: action.payload.1)
                    .map {
                        [
                            SeatActions.onResponseSeatInvitation(),
                        ]
                    }
                    .catch { error in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchSeatApplication = SeatEffect.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: SeatActions.fetchSeatApplicationList)
            .flatMap { _ in
                environment.seatService.fetchSeatApplicationList()
                    .map { result in
                        SeatActions.onFetchSeatApplicationList(payload: result)
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
                environment.seatService.responseRemoteRequest(isAgree: action.payload.0, requestId: action.payload.1)
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
    
    let cancelSeatApplication = SeatEffect.dispatchingOne { actions, environment in
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
    
    static func handleInviteTakeSeat(result: TakeSeatResult, environment: Environment) -> [Action] {
        var actions: [Action] = []
        guard let store = environment.store else { return actions }
        let seatInvitationMap = store.selectCurrent(SeatSelectors.getSeatInvitationMap)
        switch result {
        case .accepted(_, let userId):
            if let seatInvitation = seatInvitationMap.first(where: { $0.key == userId }).map({ $0.value }) {
                let message = String.localizedReplace(.inviteLinkAcceptText, replace: seatInvitation.userName)
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: message)))
                actions.append(SeatActions.removeSeatInvitation(payload: seatInvitation))
            }
        case .rejected(_, let userId, _):
            if let seatInvitation = seatInvitationMap.first(where: { $0.key == userId }).map({ $0.value }) {
                let message = String.localizedReplace(.inviteLinkRejectText, replace: seatInvitation.userName)
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: message)))
                actions.append(SeatActions.removeSeatInvitation(payload: seatInvitation))
            }
        case .timeout(_, let userId):
            if let seatInvitation = seatInvitationMap.first(where: { $0.key == userId }).map({ $0.value }) {
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .inviteLinkTimeoutText)))
                actions.append(SeatActions.removeSeatInvitation(payload: seatInvitation))
            }
        case .cancel(_, let userId):
            if let seatInvitation = seatInvitationMap.first(where: { $0.key == userId }).map({ $0.value }) {
                actions.append(SeatActions.removeSeatInvitation(payload: seatInvitation))
            }
        }
        return actions
    }
}

fileprivate extension String {
    static let operationSuccessful = localized("live.error.success")
    static let takeSeatSuccess = localized("live.seat.takeSeatSuccess")
    static let takeSeatApplicationRejected = localized("live.seat.takeSeatApplicationRejected")
    static let takeSeatApplicationTimeout = localized("live.seat.takeSeatApplicationTimeout")
    static let takeSeatApplyingToast = localized("live.seat.takeSeatApplying")
    static let inviteLinkAcceptText = localized("live.seat.inviteSeatSuccess.xxx")
    static let inviteLinkRejectText = localized("live.seat.inviteSeatRejected.xxx")
    static let inviteLinkTimeoutText = localized("live.seat.inviteSeatTimeout")
}
