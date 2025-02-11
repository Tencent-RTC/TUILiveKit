//
//  LSCoGuestManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/21.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

class LSCoGuestManager {
    private let observerState = ObservableState<LSCoGuestState>(initialState: LSCoGuestState())
    var coGuestState: LSCoGuestState {
        observerState.state
    }
    
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: LSRoomEngineService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { coGuestState in
            coGuestState = LSCoGuestState()
        }
    }
}

extension LSCoGuestManager {
    func subscribeState<Value>(_ selector: StateSelector<LSCoGuestState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

extension LSCoGuestManager {
    func fetchSeatList() {
        Task {
            do {
                let seatList = try await service.getSeatList()
                initSeatList(seatList: seatList)
                if isSelfInSeat() {
                    update{ coGuestState in
                        coGuestState.coGuestStatus = .linking
                    }
                }
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    func fetchSeatApplicationList() {
        Task {
            do {
                let requestList = try await service.getSeatApplicationList()
                initSeatApplicationList(list: requestList)
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    func removeSeatApplication(userId: String) {
        update { coGuestState in
            if let requestToRemove = coGuestState.requestCoGuestList.first(where: { $0.userId == userId }) {
                coGuestState.requestCoGuestList.remove(requestToRemove)
            }
        }
    }
    
    func update(coGuestState: LSCoGuestStateUpdateClosure) {
        observerState.update(reduce: coGuestState)
    }
    
    func update(coGuestStatus: CoGuestStatus) {
        observerState.update { state in
            state.coGuestStatus = coGuestStatus
        }
    }
}

// MARK: - Observer
extension LSCoGuestManager {
    func onSeatListChanged(userList: [TUIUserInfo], joinList: [TUIUserInfo], leaveList: [TUIUserInfo]) {
        initSeatList(userList: userList)
        if joinList.first(where: { isSelfInfo(userId: $0.userId) }) != nil {
            update { coGuestState in
                coGuestState.coGuestStatus = .linking
            }
        }
        if leaveList.first(where: { isSelfInfo(userId: $0.userId) }) != nil {
            update { coGuestState in
                coGuestState.coGuestStatus = .none
            }
        }
    }
    
    func onRequestReceived(inviter: TUIUserInfo) {
        addSeatApplication(inviter: inviter)
    }
    
    func onRequestCancelled(inviter: TUIUserInfo) {
        removeSeatApplication(userId: inviter.userId)
        update{ coGuestState in
            coGuestState.coGuestStatus = .none
        }
    }
    
    func onUserConnectionAccepted(userId: String) {
        guard let context = context else { return }
        let ownerId = context.roomManager.roomState.ownerInfo.userId
        let selfId = context.userManager.userState.selfInfo.userId
        if ownerId != selfId {
            update{ coGuestState in
                coGuestState.coGuestStatus = .linking
            }
        }
    }
    
    func onUserConnectionRejected(userId: String) {
        update{ coGuestState in
            coGuestState.coGuestStatus = .none
        }
        toastSubject.send(.takeSeatApplicationRejected)
    }
    
    func onUserConnectionTimeout(userId: String) {
        update{ coGuestState in
            coGuestState.coGuestStatus = .none
        }
        toastSubject.send(.takeSeatApplicationTimeout)
    }
    
    func onKickedOffSeat() {
        toastSubject.send(.kickedOutOfSeat)
    }
}

// MARK: - Private
extension LSCoGuestManager {
    private func initSeatList(seatList: [TUISeatInfo]) {
        var newList: [LSSeatInfo] = []

        for info in seatList {
            guard let userId = info.userId, !userId.isEmpty else {
                continue
            }
            let seatInfo = LSSeatInfo(info: info)
            newList.append(seatInfo)
        }
        
        update { coGuestState in
            coGuestState.connectedUserList = newList
        }
    }
    
    private func initSeatList(userList: [TUIUserInfo]) {
        var newList: [LSSeatInfo] = []

        for info in userList {
            guard !info.userId.isEmpty else {
                continue
            }
            let seatInfo = LSSeatInfo(userInfo: info)
            newList.append(seatInfo)
        }
        
        update { coGuestState in
            coGuestState.connectedUserList = newList
        }
    }
    
    private func initSeatApplicationList(list: [TUIRequest]) {
        var newList: [LSSeatApplication] = []

        for request in list {
            let seatInfo = LSSeatApplication(request: request)
            newList.append(seatInfo)
        }
        
        update { coGuestState in
            coGuestState.requestCoGuestList = Set(newList)
        }
    }
    
    private func addSeatApplication(inviter: TUIUserInfo) {
        let seatApplication = LSSeatApplication(userInfo: inviter)
        update { coGuestState in
            coGuestState.requestCoGuestList.insert(seatApplication)
        }
    }
    
    private func isSelfInSeat() -> Bool {
        guard let context = context else { return false }
        let selfUserId = context.userManager.userState.selfInfo.userId
        return coGuestState.connectedUserList.contains{ $0.userId == selfUserId }
    }
    
    private func isSelfInfo(userId: String) -> Bool {
        guard let context = context else { return false }
        return context.userManager.userState.selfInfo.userId == userId
    }
}

fileprivate extension String {
    static let takeSeatApplicationRejected = localized("live.seat.takeSeatApplicationRejected")
    static let takeSeatApplicationTimeout = localized("live.seat.takeSeatApplicationTimeout")
    static let kickedOutOfSeat = localized("live.seat.kickedOutOfSeat")
}
