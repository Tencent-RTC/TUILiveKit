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

class VRRoomManager: VRRoomManagerInterface, VRLiveListObserverInterface, VRRoomEngineObserverRoomInterface {
    var state: VRRoomState {
        observerState.state
    }
    
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

// MARK: - VRRoomManagerInterface
extension VRRoomManager {
    func fetchRoomInfo() {
        Task {
            do {
                guard let service = service else { return }
                let roomInfo = try await service.fetchRoomInfo()
                update { state in
                    state.roomId = roomInfo.roomId
                    state.roomName = roomInfo.name
                    state.ownerInfo.userId = roomInfo.ownerId
                    state.seatMode = roomInfo.seatMode
                    state.createTime = roomInfo.createTime
                    state.maxSeatCount = roomInfo.maxSeatCount
                }
            } catch let error {
                toastSubject.send(error.localizedDescription)
            }
        }
    }
    
    func fetchLiveInfo(roomId: String) {
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
            } catch let error {
                toastSubject.send(error.localizedDescription)
            }
        }
    }
    
    func fetchRoomOwnerInfo(ownerId: String) {
        Task {
            do {
                guard let service = service else { return }
                let user = try await service.fetchRoomOwnerInfo(ownerId: ownerId)
                update { state in
                    state.ownerInfo = user
                }
            } catch let error {
                toastSubject.send(error.localizedDescription)
            }
        }
    }
    
    func setRoomSeatModeByAdmin(_ mode: TUISeatMode) {
        Task {
            do {
                guard let service = service else { return }
                try await service.setRoomSeatModeByAdmin(mode)
                update { state in
                    state.seatMode = mode
                }
            } catch let error {
                toastSubject.send(error.localizedDescription)
            }
        }
    }
    
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) {
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
            } catch let error {
                toastSubject.send(error.localizedDescription)
            }
        }
    }
    
    func update(roomId: String) {
        update { state in
            state.roomId = roomId
        }
    }
    
    func update(roomName: String) {
        update { state in
            state.roomName = roomName
        }
    }
    
    func update(ownerInfo: VRUser) {
        update { state in
            state.ownerInfo = ownerInfo
        }
    }
    
    func update(roomInfo: TUIRoomInfo) {
        update { state in
            state.roomId = roomInfo.roomId
            state.roomName = roomInfo.name
            state.ownerInfo.userId = roomInfo.ownerId
            state.seatMode = roomInfo.seatMode
            state.createTime = roomInfo.createTime
            state.maxSeatCount = roomInfo.maxSeatCount
        }
    }
    
    func update(giftIncome: Int, giftPeople: String) {
        update { state in
            state.liveExtraInfo.giftIncome += giftIncome
            state.liveExtraInfo.giftPeopleSet.insert(giftPeople)
        }
    }
    
    func update(roomParams: RoomParams) {
        update { state in
            state.maxSeatCount = roomParams.maxSeatCount
            state.seatMode = roomParams.seatMode
        }
    }
    
    func update(roomCategory: LiveStreamCategory) {
        update { state in
            state.liveExtraInfo.category = roomCategory
        }
    }
    
    func update(roomPrivacy: LiveStreamPrivacyStatus) {
        update { state in
            state.liveExtraInfo.liveMode = roomPrivacy
        }
    }
    
    func update(roomCoverUrl: String) {
        update { state in
            state.coverURL = roomCoverUrl
        }
    }
    
    func update(backgroundUrl: String) {
        update { state in
            state.backgroundURL = backgroundUrl
        }
    }
    
    func update(seatMode: TUISeatMode) {
        update { state in
            state.seatMode = seatMode
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
    
    func onRoomSeatModeChanged(roomId: String, seatMode: TUISeatMode) {
        update(roomState: { state in
            state.seatMode = seatMode
        })
    }
    
    func onRoomDismissed(roomId: String) {
        context?.exitSubject.send()
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        update(roomState: { state in
            state.userCount = userCount > 0 ? userCount - 1 : userCount
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
