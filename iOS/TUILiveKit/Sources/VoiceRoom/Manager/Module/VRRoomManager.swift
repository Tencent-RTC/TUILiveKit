//
//  VRRoomManager.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine

class VRRoomManager: VRLiveListObserverInterface, VRRoomEngineObserverRoomInterface {
    var state: VRRoomState {
        observerState.state
    }
    var roomParams = RoomParams()
    
    private typealias Context = VoiceRoomManager.Context
    private let observerState = ObservableState<VRRoomState>(initialState: VRRoomState())
    private weak var context: Context?
    private weak var service: VRRoomService? {
        context?.service.roomService
    }
    private let toastSubject: PassthroughSubject<String, Never>
    required init(context: VoiceRoomManager.Context) {
        self.context = context
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { state in
            state.roomId = ""
            state.createTime = 0
            state.roomName = ""
            state.userCount = 0
            state.liveExtraInfo.category = .chat
            state.liveExtraInfo.liveMode = .public
            state.liveExtraInfo.maxAudienceCount = 0
            state.liveExtraInfo.messageCount = 0
            state.liveExtraInfo.giftTotalCoins = 0
            state.liveExtraInfo.giftTotalUniqueSender = 0
            state.liveExtraInfo.likeTotalUniqueSender = 0
        }
    }
}

extension VRRoomManager {
    func subscribeState<Value>(_ selector: StateSelector<VRRoomState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeState() -> AnyPublisher<VRRoomState, Never> {
        return observerState.subscribe()
    }
}

extension VRRoomManager {
    func prepareRoomIdBeforeEnterRoom(roomId: String, roomParams: RoomParams?) {
        update { state in
            state.roomId = roomId
        }
        if let param = roomParams {
            self.roomParams = param
        }
    }
    
    func onJoinVoiceRoom(liveInfo: TUILiveInfo) {
        guard let context = context else { return }
        update(liveInfo: liveInfo)
    }
    
    func onStartVoiceRoom(liveInfo: TUILiveInfo) {
        guard let context = context else { return }
        update(liveInfo: liveInfo)
        setLiveInfo(liveInfo: liveInfo, modifyFlag: [.coverUrl, .publish, .category, .backgroundUrl])
    }
    
    func fetchGiftCount(roomId: String) async throws {
        guard let service = service else { return }
        let result = try await service.fetchGiftCount(roomId: roomId)
        update { state in
            state.liveExtraInfo.giftTotalCoins = Int(result.totalGiftCoins)
            state.liveExtraInfo.giftTotalUniqueSender = Int(result.totalUniqueGiftSenders)
        }
    }
    
    func fetchLikeCount(roomId: String) async throws {
        guard let service = service else { return }
        let result = try await service.fetchLikeCount(roomId: roomId)
        update { state in
            state.liveExtraInfo.likeTotalUniqueSender = Int(result)
        }
    }
    
    func fetchViewCount(roomId: String) async throws {
        guard let service = service else { return }
        let result = try await service.fetchLiveInfo(roomId: roomId)
        update { state in
            state.liveExtraInfo.maxAudienceCount = result.viewCount
        }
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
    
    func onSetRoomCoverUrl(_ coverUrl: String) {
        update { state in
            state.coverURL = coverUrl
        }
    }
    
    func onSetRoomBackgroundUrl(_ backgroundUrl: String, isSetToService: Bool = false) {
        update { state in
            state.backgroundURL = backgroundUrl
        }
        if isSetToService {
            let liveInfo = TUILiveInfo()
            liveInfo.roomInfo.roomId = state.roomId
            liveInfo.backgroundUrl = backgroundUrl
            setLiveInfo(liveInfo: liveInfo, modifyFlag: .backgroundUrl)
        }
    }
}

// MARK: - VRRoomManagerInterface
extension VRRoomManager {
    
    private func fetchLiveInfo(roomId: String) {
        Task {
            do {
                guard let service = service else { return }
                let liveInfo = try await service.fetchLiveInfo(roomId: roomId)
            } catch let error as InternalError {
                toastSubject.send(error.localizedMessage)
            }
        }
    }
    
    private func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        Task {
            do {
                guard let service = service else { return }
                try await service.setLiveInfo(liveInfo: liveInfo, modifyFlag: modifyFlag)
                update { state in
                    if modifyFlag.contains(.coverUrl) {
                        state.coverURL = liveInfo.coverUrl
                    }
                    if modifyFlag.contains(.category) {
                        if let first = liveInfo.categoryList.first {
                            switch first.intValue {
                            case 0:
                                state.liveExtraInfo.category = .chat
                            case 1:
                                state.liveExtraInfo.category = .beauty
                            case 2:
                                state.liveExtraInfo.category = .teach
                            case 3:
                                state.liveExtraInfo.category = .shopping
                            case 4:
                                state.liveExtraInfo.category = .music
                            default:
                                break
                            }
                        }
                    }
                    if modifyFlag.contains(.publish) {
                        state.liveExtraInfo.liveMode = liveInfo.isPublicVisible ? .public : .privacy
                    }
                    if modifyFlag.contains(.backgroundUrl) {
                        state.backgroundURL = liveInfo.backgroundUrl
                    }
                }
            } catch let error as InternalError {
                toastSubject.send(error.localizedMessage)
            }
        }
    }
    
    private func update(liveInfo: TUILiveInfo) {
        update { state in
            state.roomId = liveInfo.roomId
            state.roomName = liveInfo.name
            state.createTime = liveInfo.createTime
            state.liveInfo = liveInfo
            state.backgroundURL = liveInfo.backgroundUrl
            state.coverURL = liveInfo.coverUrl
        }
    }
}

// MARK: - VRLiveListObserverInterface
extension VRRoomManager {
    func onLiveInfoChanged(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
        update(roomState: { state in
            if modifyFlag.contains(.coverUrl) {
                state.coverURL = liveInfo.coverUrl
            }
            if modifyFlag.contains(.category) {
                if let first = liveInfo.categoryList.first {
                    switch first.intValue {
                    case 0:
                        state.liveExtraInfo.category = .chat
                    case 1:
                        state.liveExtraInfo.category = .beauty
                    case 2:
                        state.liveExtraInfo.category = .teach
                    case 3:
                        state.liveExtraInfo.category = .shopping
                    case 4:
                        state.liveExtraInfo.category = .music
                    default:
                        break
                    }
                }
            }
            if modifyFlag.contains(.publish) {
                state.liveExtraInfo.liveMode = liveInfo.isPublicVisible ? .public : .privacy
            }
            if modifyFlag.contains(.backgroundUrl) {
                state.backgroundURL = liveInfo.backgroundUrl
            }
        })
    }
}

// MARK: - VRRoomEngineObserverRoomInterface
extension VRRoomManager {
    func onRoomNameChanged(roomId: String, roomName: String) {
        update(roomState: { state in
            state.roomId = roomId
            state.roomName = roomName
        })
    }
    
    func onRoomDismissed(roomId: String) {
        context?.exitSubject.send()
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        update(roomState: { state in
            state.userCount = userCount > 0 ? userCount - 1 : userCount
            if userCount > state.liveExtraInfo.maxAudienceCount {
                state.liveExtraInfo.maxAudienceCount = userCount - 1
            }
        })
    }
    
    func onKickedOffLine(message: String) {
        context?.exitSubject.send()
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        context?.exitSubject.send()
    }
}

// MARK: - Private functions
extension VRRoomManager {
    private typealias RoomStateUpdateClosure = (inout VRRoomState) -> Void

    private func update(roomState: RoomStateUpdateClosure) {
        observerState.update(reduce: roomState)
    }
}
