//
//  AudienceCoGuestManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/3/7.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import LiveStreamCore

class AudienceCoGuestManager {
    private let observerState = ObservableState<AudienceCoGuestState>(initialState: AudienceCoGuestState())
    var coGuestState: AudienceCoGuestState {
        observerState.state
    }
    
    private typealias Context = AudienceManager.Context
    private weak var context: Context?
    private let service: AudienceService
    
    init(context: AudienceManager.Context) {
        self.context = context
        self.service = context.service
    }
}

extension AudienceCoGuestManager {
    private func update(coGuestState: AudienceCoGuestStateUpdateClosure) {
        observerState.update(reduce: coGuestState)
    }
}

// MARK: - Interface
extension AudienceCoGuestManager {
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
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        guard let context = context else { return }
        updateMediaLockStatus(seatList: seatList)
        let userId = context.coreUserState.selfInfo.userId
        let isLeftListContainsSelf = leftList.contains(where: { $0.userId == userId })
        let isSeatedListContainsSelf = seatedList.contains(where: { $0.userId == userId })
        let isSeatMoved = isLeftListContainsSelf && isSeatedListContainsSelf
        guard !isSeatMoved else { return }
        if isLeftListContainsSelf {
            context.mediaManager.onSelfLeaveSeat()
            observerState.update { state in
                state.coGuestStatus = .none
            }
        }
        if isSeatedListContainsSelf {
            observerState.update { state in
                state.coGuestStatus = .linking
            }
        }
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AudienceCoGuestState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Private
extension AudienceCoGuestManager {
    
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
