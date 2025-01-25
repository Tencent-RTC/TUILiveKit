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
    
    func resetState() {
        update { state in
            state = LSRoomState()
        }
    }
}

extension LSRoomManager {
    func subscribeState<Value>(_ selector: StateSelector<LSRoomState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Interface
extension LSRoomManager {
    func update(roomId: String) {
        update { state in
            state.roomId = roomId
        }
    }
    
    func update(roomState: LSRoomStateUpdateClosure) {
        observerState.update(reduce: roomState)
    }
    
    func update(liveStatus: LiveStatus) {
        observerState.update { state in
            state.liveStatus = liveStatus
        }
    }
    
    func update(roomCategory: LiveStreamCategory) {
        observerState.update { state in
            state.liveExtraInfo.category = roomCategory
        }
    }
    
    func update(roomPrivacy: LiveStreamPrivacyStatus) {
        observerState.update { state in
            state.liveExtraInfo.liveMode = roomPrivacy
        }
    }
    
    func update(roomCoverUrl: String) {
        observerState.update { state in
            state.coverURL = roomCoverUrl
        }
    }
    
    func getDefaultRoomName() -> String {
        guard let context = context else { return "" }
        return context.userManager.userState.selfInfo.name.isEmpty ?
        context.userManager.userState.selfInfo.userId :
        context.userManager.userState.selfInfo.name
    }
    
    func updateRoomState(roomInfo: TUIRoomInfo) {
        update { state in
            state.roomId = roomInfo.roomId
            state.createTime = roomInfo.createTime
            state.roomName = roomInfo.name
            state.ownerInfo.userId = roomInfo.ownerId
            state.ownerInfo.name = roomInfo.ownerName
            state.ownerInfo.avatarUrl = roomInfo.ownerAvatarUrl
            state.maxSeatCount = roomInfo.maxSeatCount
        }
    }
    
    func syncLiveInfoToService() {
        let liveInfo = TUILiveInfo()
        liveInfo.roomInfo.roomId = roomState.roomId
        liveInfo.coverUrl = roomState.coverURL
        liveInfo.isPublicVisible = roomState.liveExtraInfo.liveMode == .public
        liveInfo.categoryList = [NSNumber(value: roomState.liveExtraInfo.category.rawValue)]
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
}

// MARK: - Observer
extension LSRoomManager {
    func onLiveEnd() {
        update { state in
            state.liveStatus = .finished
        }
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
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
    func updateLiveInfo(liveInfo: TUILiveInfo,
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
            if modifyFlag.contains(.category), let category = liveInfo.categoryList.first {
                state.liveExtraInfo.category = LiveStreamCategory(rawValue: category.intValue) ?? .chat
            }
        }
    }
}
