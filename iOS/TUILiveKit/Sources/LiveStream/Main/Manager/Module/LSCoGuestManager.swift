//
//  LSCoGuestManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/3/7.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import TUILiveResources
import LiveStreamCore

class LSCoGuestManager {
    private let observerState = ObservableState<LSCoGuestState>(initialState: LSCoGuestState())
    var coGuestState: LSCoGuestState {
        observerState.state
    }
    
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    private let service: LSRoomEngineService
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
        subscribeCoreCoGuestState()
    }
    
    private func subscribeCoreCoGuestState() {
        context?.provider?.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.seatList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink(receiveValue: { [weak self] seatList in
                guard let self = self else { return }
                updateCoGuestStatusBySeatList(seatList: seatList)
                updateMediaLockStatus(seatList: seatList)
                if seatList.first(where: { $0.userId == self.context?.coreUserState.selfInfo.userId }) == nil {
                    context?.mediaManager.onSelfLeaveSeat()
                }
            })
            .store(in: &cancellableSet)
    }
}

extension LSCoGuestManager {
    private func update(coGuestState: LSCoGuestStateUpdateClosure) {
        observerState.update(reduce: coGuestState)
    }
}

// MARK: - Interface
extension LSCoGuestManager {
    func onLockMediaStatusBtnClicked(userId: String, lockParams: TUISeatLockParams) async throws {
        if let seatIndex = context?.coreCoGuestState.seatList.first(where: { $0.userId == userId })?.index {
            try await service.lockSeatByAdmin(seatIndex: seatIndex, lockParams: lockParams)
        } else {
            throw InternalError(error: LiveError.userNotInSeat, message: LiveError.userNotInSeat.description)
        }
    }
    
    func onStartRequestIntraRoomConnection() {
        observerState.update { state in
            state.coGuestStatus = .applying
        }
    }
    
    func onRequestIntraRoomConnectionFailed() {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func onUserConnectionRejected(userId: String) {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func onUserConnectionTimeout(userId: String) {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func onKickedOffSeat() {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func onStartCancelIntraRoomConnection() {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func onCancelIntraRoomConnection() {
        observerState.update { state in
            state.coGuestStatus = .none
        }
    }
    
    func subscribeState<Value>(_ selector: StateSelector<LSCoGuestState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Private
extension LSCoGuestManager {
    private func updateCoGuestStatusBySeatList(seatList: [TUISeatInfo]) {
        let isLinking = !seatList.filter { $0.userId == context?.coreUserState.selfInfo.userId }.isEmpty
        observerState.update { state in
            state.coGuestStatus = isLinking ? .linking : .none
        }
    }
    
    private func updateMediaLockStatus(seatList: [TUISeatInfo]) {
        for seatInfo in seatList {
            guard let userId = seatInfo.userId, !userId.isEmpty else { continue }
            update { state in
                if seatInfo.isAudioLocked {
                    state.lockAudioUserList.insert(userId)
                } else {
                    state.lockAudioUserList.remove(userId)
                }
                if seatInfo.isVideoLocked {
                    state.lockVideoUserList.insert(userId)
                } else {
                    state.lockVideoUserList.remove(userId)
                }
            }
            if userId == context?.coreUserState.selfInfo.userId {
                context?.mediaManager.onSelfMediaDeviceStateChanged(seatInfo: seatInfo)
            }
        }
    }
}
