//
//  LSRoomManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import TUILiveComponent

class LSRoomManager {
    private let observerState = ObservableState<LSRoomState>(initialState: LSRoomState())
    var roomState: LSRoomState {
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
}

// MARK: - Interface
extension LSRoomManager {
    func prepareLiveInfoBeforeEnterRoom(liveInfo: TUILiveInfo) {
        updateLiveInfo(liveInfo: liveInfo)
    }
    
    func prepareRoomIdBeforeEnterRoom(roomId: String) {
        update { state in
            state.roomId = roomId
        }
    }
    
    func onStartLive(isJoinSelf: Bool, roomInfo: TUIRoomInfo) {
        update(liveStatus: .pushing)
        updateRoomState(roomInfo: roomInfo)
        if !isJoinSelf {
            syncLiveInfoToService()
        }
    }
    
    func onJoinLive(roomInfo: TUIRoomInfo) {
        Task {
            try? await fetchLiveInfo(roomId: roomInfo.roomId)
        }
        updateRoomState(roomInfo: roomInfo)
        update(liveStatus: .playing)
    }
    
    func onStopLive() {
        update(liveStatus: .finished)
    }
    
    func onLeaveLive() {
        update { state in
            state = LSRoomState()
        }
    }
    
    func onAudienceSliderCellInit(liveInfo: LiveInfo) {
        update { state in
            state.coverURL = liveInfo.coverUrl
            state.roomId = liveInfo.roomId
            state.roomName = liveInfo.name
        }
    }
    
    func getDefaultRoomName() -> String {
        guard let context = context else { return "" }
        let selfInfo = context.coreUserState.selfInfo
        return selfInfo.userName.isEmpty ? selfInfo.userId : selfInfo.userName
    }
    
    func fetchLiveInfo(roomId: String) async throws -> TUILiveInfo {
        let liveInfo = try await service.fetchLiveInfo(roomId: roomId)
        updateLiveInfo(liveInfo: liveInfo)
        return liveInfo
    }
    
    func onSetRoomName(_ name: String) {
        update { state in
            state.roomName = name
        }
    }
    
    func onSetRoomPrivacy(_ mode: LiveStreamPrivacyStatus) {
        update { state in
            state.liveExtraInfo.liveMode = mode
        }
    }
    
    func onSetRoomCoverUrl(_ url: String) {
        update { state in
            state.coverURL = url
        }
    }
    
    func onReceiveGift(price: Int, senderUserId: String) {
        update { state in
            state.liveExtraInfo.giftIncome += price
            state.liveExtraInfo.giftPeopleSet.insert(senderUserId)
        }
    }
    
    func subscribeState<Value>(_ selector: StateSelector<LSRoomState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Observer
extension LSRoomManager {
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
}

// MARK: - Private functions
extension LSRoomManager {
    private func update(roomState: LSRoomStateUpdateClosure) {
        observerState.update(reduce: roomState)
    }
    
    private func update(liveStatus: LiveStatus) {
        update { state in
            state.liveStatus = liveStatus
        }
    }
    
    private func updateRoomState(roomInfo: TUIRoomInfo) {
        update { state in
            state.roomId = roomInfo.roomId
            state.createTime = roomInfo.createTime
            state.roomName = roomInfo.name
            state.roomInfo = roomInfo
        }
    }
    
    private func syncLiveInfoToService() {
        let liveInfo = TUILiveInfo()
        liveInfo.roomInfo.roomId = roomState.roomId
        liveInfo.coverUrl = roomState.coverURL
        liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode == .public
        Task {
            var modifyFlag: TUILiveModifyFlag = []
            modifyFlag = modifyFlag.union([.coverUrl, .publish, .category, .backgroundUrl])
            do {
                try await service.syncLiveInfoToService(liveInfo: liveInfo, modifyFlag: modifyFlag)
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    private func updateLiveInfo(liveInfo: TUILiveInfo,
                                updateRoomInfo: Bool = true,
                                modifyFlag: TUILiveModifyFlag = [.activityStatus, .category, .publish, .coverUrl]) {
        if updateRoomInfo {
            updateRoomState(roomInfo: liveInfo.roomInfo)
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
