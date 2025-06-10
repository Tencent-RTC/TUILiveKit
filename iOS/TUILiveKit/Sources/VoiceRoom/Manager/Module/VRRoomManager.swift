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
import TUILiveResources

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
            state.liveExtraInfo.giftIncome = 0
            state.liveExtraInfo.giftPeopleSet = []
            state.liveExtraInfo.likeCount = 0
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
    
    func onJoinVoiceRoom(roomInfo: TUIRoomInfo) {
        update(roomInfo: roomInfo)
        guard let context = context else { return }
        if context.coreUserState.selfInfo.userId != roomInfo.ownerId {
            fetchLiveInfo(roomId: roomInfo.roomId)
        }
    }
    
    func onStartVoiceRoom(roomInfo: TUIRoomInfo) {
        guard let context = context else { return }
        update(roomInfo: roomInfo)
        
        let liveInfo = TUILiveInfo()
        liveInfo.roomInfo.roomId = context.roomManager.state.roomId
        liveInfo.coverUrl = context.roomManager.state.coverURL
        liveInfo.backgroundUrl = context.roomManager.state.backgroundURL
        liveInfo.categoryList = [NSNumber(value: context.roomManager.state.liveExtraInfo.category.rawValue)]
        liveInfo.isPublicVisible = context.roomManager.state.liveExtraInfo.liveMode == .public
        setLiveInfo(liveInfo: liveInfo, modifyFlag: [.coverUrl, .publish, .category, .backgroundUrl])
    }
    
    func onReceiveGift(price: Int, senderUserId: String) {
        update { state in
            state.liveExtraInfo.giftIncome += price
            state.liveExtraInfo.giftPeopleSet.insert(senderUserId)
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
    private func fetchRoomInfo() {
        Task {
            do {
                guard let service = service else { return }
                let roomInfo = try await service.fetchRoomInfo()
                update { state in
                    state.roomId = roomInfo.roomId
                    state.roomName = roomInfo.name
                    state.createTime = roomInfo.createTime
                    state.roomInfo = roomInfo
                }
            } catch let error as InternalError {
                toastSubject.send(error.localizedMessage)
            }
        }
    }
    
    private func fetchLiveInfo(roomId: String) {
        Task {
            do {
                guard let service = service else { return }
                let liveInfo = try await service.fetchLiveInfo(roomId: roomId)
                if let categoryValue = liveInfo.categoryList.first?.intValue,
                   let category = LiveStreamCategory(rawValue: categoryValue) {
                    update { state in
                        state.liveExtraInfo.category = category
                    }
                }
                update { state in
                    state.coverURL = liveInfo.coverUrl
                    state.liveExtraInfo.liveMode = liveInfo.isPublicVisible ? .public : .privacy
                    state.backgroundURL = liveInfo.backgroundUrl
                }
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
    
    private func update(roomInfo: TUIRoomInfo) {
        update { state in
            state.roomId = roomInfo.roomId
            state.roomName = roomInfo.name
            state.createTime = roomInfo.createTime
            state.roomInfo = roomInfo
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
