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
import TUILiveComponent

class VRSeatManager {
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

extension VRSeatManager {
    func onSentTakeSeatRequest() {
        update { seatState in
            seatState.isApplyingToTakeSeat = true
        }
    }
    
    func onRespondedTakeSeatRequest() {
        update { seatState in
            seatState.isApplyingToTakeSeat = false
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
    
    func onRespondedRemoteRequest() {
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
    
    func onApplyToTakeSeatRequestReceived(userInfo: TUIUserInfo) {
        update { seatState in
            if !seatState.seatApplicationList.contains(where: { $0.userId == userInfo.userId }) {
                seatState.seatApplicationList.append(VRSeatApplication(userInfo: userInfo))
            }
        }
    }
    
    func onApplyToTakeSeatRequestCancelled(_ userInfo: TUIUserInfo) {
        update { seatState in
            seatState.seatApplicationList.removeAll(where: { $0.userId == userInfo.userId })
        }
    }
    
    func onRemoteRequestError(userId: String) {
        update { seatState in
            seatState.seatApplicationList.removeAll(where: { $0.userId == userId })
        }
    }
}

extension VRSeatManager {
    private typealias SeatStateUpdateClosure = (inout VRSeatState) -> Void

    private func update(seatState: SeatStateUpdateClosure) {
        observerState.update(reduce: seatState)
    }
}
