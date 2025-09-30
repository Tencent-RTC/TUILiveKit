//
//  AudienceRoomManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

class AudienceRoomManager {
    private let observerState = ObservableState<AudienceRoomState>(initialState: AudienceRoomState())
    var roomState: AudienceRoomState {
        observerState.state
    }
    
    private typealias Context = AudienceManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: AudienceService
    
    init(context: AudienceManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
    }
}

// MARK: - Interface
extension AudienceRoomManager {
    func prepareRoomIdBeforeEnterRoom(roomId: String) {
        update { state in
            state.roomId = roomId
        }
    }
    
    func onJoinLive(liveInfo: TUILiveInfo) {
        updateRoomState(liveInfo: liveInfo)
        update(liveStatus: .playing)
    }
    
    func onLeaveLive() {
        update { state in
            state = AudienceRoomState()
        }
    }
    
    func onAudienceSliderCellInit(liveInfo: LiveInfo) {
        update { state in
            state.coverURL = liveInfo.coverUrl
            state.backgroundUrl = liveInfo.backgroundUrl.isEmpty ? liveInfo.coverUrl : liveInfo.backgroundUrl
            state.roomId = liveInfo.roomId
            state.roomName = liveInfo.name
        }
    }
    
    func fetchLiveInfo(roomId: String) async throws -> TUILiveInfo {
        let liveInfo = try await service.fetchLiveInfo(roomId: roomId)
        updateLiveInfo(liveInfo: liveInfo)
        return liveInfo
    }
    
    func onReceiveGift(price: Int, senderUserId: String) {
        update { state in
            state.liveExtraInfo.giftIncome += price
            state.liveExtraInfo.giftPeopleSet.insert(senderUserId)
        }
    }
    
    func updateVideoStreamIsLandscape(isLandscape: Bool) {
        update { state in
            state.roomVideoStreamIsLandscape = isLandscape
        }
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AudienceRoomState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Observer
extension AudienceRoomManager {
    func onLiveEnd(roomId: String) {
        guard roomId == roomState.roomId else { return }
        update { state in
            state.liveStatus = .finished
        }
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        guard roomId == roomState.roomId else { return }
        context?.kickedOutSubject.send()
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        guard roomId == roomState.roomId else { return }
        if userCount > 0 {
            update { state in
                state.userCount = userCount - 1
                if userCount > state.liveExtraInfo.maxAudienceCount {
                    state.liveExtraInfo.maxAudienceCount = userCount - 1
                }
            }
        }
    }
    
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        updateLiveInfo(liveInfo: liveInfo, updateRoomInfo: false, modifyFlag: modifyFlag)
    }
    
    func onVideoStreamIsLandscape(isLandscape: Bool) {
        update { state in
            state.roomVideoStreamIsLandscape = isLandscape
        }
    }
}

// MARK: - Private functions
extension AudienceRoomManager {
    private func update(roomState: AudienceRoomStateUpdateClosure) {
        observerState.update(reduce: roomState)
    }
    
    private func update(liveStatus: LiveStatus) {
        update { state in
            state.liveStatus = liveStatus
        }
    }
    
    private func updateRoomState(liveInfo: TUILiveInfo) {
        update { state in
            state.roomId = liveInfo.roomId
            state.createTime = liveInfo.createTime
            state.coverURL = liveInfo.coverUrl
            state.backgroundUrl = liveInfo.backgroundUrl.isEmpty ? liveInfo.coverUrl : liveInfo.backgroundUrl
            state.roomName = liveInfo.name
            state.liveInfo = liveInfo
        }
    }
    
    private func updateLiveInfo(liveInfo: TUILiveInfo,
                                updateRoomInfo: Bool = true,
                                modifyFlag: TUILiveModifyFlag = [.activityStatus, .category, .publish, .coverUrl]) {
        if updateRoomInfo {
            updateRoomState(liveInfo: liveInfo)
        }
        update { state in
            if modifyFlag.contains(.coverUrl) {
                state.coverURL = liveInfo.coverUrl
            }
            if modifyFlag.contains(.publish) {
                state.liveExtraInfo.liveMode = liveInfo.isPublicVisible ? .public : .privacy
            }
            if modifyFlag.contains(.activityStatus) {
                state.liveExtraInfo.activeStatus = liveInfo.activityStatus
            }
        }
    }
}
