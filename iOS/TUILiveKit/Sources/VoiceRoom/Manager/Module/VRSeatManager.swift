//
//  VRSeatManager.swift
//  TUILiveKit
//
//  Created by gg on 2024/11/19.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

class VRSeatManager: VRSeatManagerInterface, VRRoomEngineObserverSeatInterface {
    var state: VRSeatState {
        observerState.state
    }
    
    private typealias Context = VoiceRoomManager.Context
    private let observerState = ObservableState<VRSeatState>(initialState: VRSeatState())
    private weak var context: Context?
    private weak var service: VRSeatService? {
        context?.service.seatService
    }
    private let toastSubject: PassthroughSubject<String, Never>
    required init(context: VoiceRoomManager.Context) {
        self.context = context
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { state in
            state.seatApplicationList = []
            state.seatList = []
        }
    }
}

extension VRSeatManager {
    func subscribeState<Value>(_ selector: StateSelector<VRSeatState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeState() -> AnyPublisher<VRSeatState, Never> {
        return observerState.subscribe()
    }
}

// MARK: - VRSeatManagerInterface
extension VRSeatManager {
    func fetchSeatList() {
        Task {
            guard let service = service else { return }
            do {
                let list = try await service.getSeatList()
                let res = list.map { VRSeatInfo(info: $0) }
                update { state in
                    state.seatList = res
                }
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    func fetchSeatApplicationList() {
        Task {
            guard let service = service else { return }
            do {
                let list = try await service.fetchSeatApplicationList()
                update { state in
                    state.seatApplicationList = list
                }
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    func addSeatUserInfo(_ info: TUIUserInfo) {
        update { seatState in
            if !seatState.seatApplicationList.contains(where: { $0.userId == info.userId }) {
                seatState.seatApplicationList.append(VRSeatApplication(userInfo: info))
            }
        }
    }
    
    func removeSeatUserInfo(_ info: TUIUserInfo) {
        update { seatState in
            seatState.seatApplicationList.removeAll(where: { $0.userId == info.userId })
        }
    }
    
    func onSentSeatInvitation(to userId: String) {
        update { seatState in
            seatState.invitedUserIds.insert(userId)
        }
    }
    
    func onRespondedSeatInvitation(of userId: String) {
        update { seatState in
            seatState.invitedUserIds.remove(userId)
        }
    }
    
    func update(applicationStateIsApplying: Bool) {
        update { seatState in
            seatState.isApplyingToTakeSeat = applicationStateIsApplying
        }
    }
}

// MARK: - VRRoomEngineObserverSeatInterface
extension VRSeatManager {
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        update(seatState: { state in
            state.seatList = seatList.map { VRSeatInfo(info: $0) }
        })
    }
}

extension VRSeatManager {
    private typealias SeatStateUpdateClosure = (inout VRSeatState) -> Void

    private func update(seatState: SeatStateUpdateClosure) {
        observerState.update(reduce: seatState)
    }
}
