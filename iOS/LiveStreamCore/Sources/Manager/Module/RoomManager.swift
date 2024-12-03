//
//  RoomManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/23.
//

import RTCCommon
import RTCRoomEngine

class RoomManager {
    let observerState = ObservableState<RoomState>(initialState: RoomState())
    var roomState: RoomState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func initCreateRoomState(roomId: String, maxCount: Int) {
        guard let context = context else { return }
        observerState.update(isPublished: false) { roomState in
            roomState.roomId = roomId
            roomState.maxCoGuestCount = maxCount
            roomState.ownerInfo.userId = context.userManager.userState.selfInfo.userId
            roomState.ownerInfo.userName = context.userManager.userState.selfInfo.userName
            roomState.ownerInfo.avatarUrl = context.userManager.userState.selfInfo.avatarUrl
        }
    }
    
    func startLive(roomInfo: TUIRoomInfo) async throws -> TUIRoomInfo {
        if roomInfo.roomId.isEmpty {
            throw LiveStreamCoreError.error(code: TUIError.invalidParameter, message: "roomId is Empty")
        }
        roomInfo.roomType = .live
        roomInfo.isSeatEnabled = true
        roomInfo.seatMode = .applyToTake
        
        context?.coHostManager.setEnableConnection(enable: true)
        context?.coGuestManager.setEnableConnection(enable: true)
        initCreateRoomState(roomId: roomInfo.roomId, maxCount: roomInfo.maxSeatCount)
        dataReport()
        do {
            try await service.createRoom(roomInfo: roomInfo)
            let roomInfo = try await service.enterRoom(roomId: roomInfo.roomId)
            onStartLiveSuccess(roomInfo: roomInfo)
            return roomInfo
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","create:[roomId:\(roomInfo.roomId),code:\(code),message:\(message)]")
            onLeaveRoom()
            throw LiveStreamCoreError.error(code: code, message: message)
        }
    }
    
    func joinLive(roomId: String) async throws -> TUIRoomInfo {
        dataReport()
        do {
            let roomInfo = try await service.enterRoom(roomId: roomId)
            onJoinLiveSuccess(roomInfo: roomInfo)
            return roomInfo
        } catch let LiveStreamCoreError.error(code, message){
            LiveStreamLog.error("\(#file)","\(#line)","create:[roomId:\(roomId),code:\(code),message:\(message)]")
            onLeaveRoom()
            throw LiveStreamCoreError.error(code: code, message: message)
        }
    }
    
    func leaveLive() async throws {
        do {
            try await service.exitRoom()
            onLeaveRoom()
        } catch let LiveStreamCoreError.error(code, message){
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func stopLive() async throws {
        do {
            try await service.destroyRoom()
            onLeaveRoom()
        } catch let LiveStreamCoreError.error(code, message){
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func updateOwnerInfo(roomInfo: TUIRoomInfo) {
        observerState.update(isPublished: false) { roomState in
            roomState.ownerInfo.userId = roomInfo.ownerId
            roomState.ownerInfo.userName = roomInfo.ownerName
            roomState.ownerInfo.avatarUrl = roomInfo.ownerAvatarUrl
        }
    }
}

// MARK: - Observer
extension RoomManager {
    func onRoomDismissed(roomId: String) {
        onLeaveRoom()
    }
}

// MARK: - Private
extension RoomManager {
    private func dataReport() {
        do {
            let apiParams: [String : Any] = [
                "api": "setFramework",
                "params": [
                    "framework": Constants.DataReport.framework,
                    "component": Constants.DataReport.component,
                    "language": Constants.DataReport.language,
                ],
              ]
            let jsonData = try JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                service.callExperimentalAPI(jsonStr: jsonString)
            } else {
                print("Error converting JSON data to string")
            }
        } catch {
            print("Error converting dictionary to JSON: \(error.localizedDescription)")
        }
    }
    
    private func updateRoomState(roomInfo: TUIRoomInfo) {
        observerState.update(isPublished: false) { roomState in
            roomState.roomId = roomInfo.roomId
            roomState.ownerInfo.userId = roomInfo.ownerId
            roomState.maxCoGuestCount = roomInfo.maxSeatCount
        }
    }
    
    private func onStartLiveSuccess(roomInfo: TUIRoomInfo) {
        modifyRoomState(value: .pushing, keyPath: \RoomState.liveStatus, isPublished: true)
        context?.userManager.onStartLiveSuccess()
        updateRoomState(roomInfo: roomInfo)
        updateOwnerInfo(roomInfo: roomInfo)
        Task {
            try? await context?.coGuestManager.initConnectedGuestList()
        }
        Task {
            try? await context?.coGuestManager.initGuestApplicationList()
        }
    }
    
    private func onJoinLiveSuccess(roomInfo: TUIRoomInfo) {
        updateRoomState(roomInfo: roomInfo)
        modifyRoomState(value: .playing, keyPath: \RoomState.liveStatus, isPublished: true)
        if context?.userManager.userState.selfInfo.userId == roomInfo.ownerId {
            context?.userManager.onStartLiveSuccess()
        }
        updateOwnerInfo(roomInfo: roomInfo)
        Task {
            try? await context?.coGuestManager.initConnectedGuestList()
        }
    }
    
    private func onLeaveRoom() {
        modifyRoomState(value: .none, keyPath: \RoomState.liveStatus, isPublished: true)
        modifyRoomState(value: TUIUserInfo(), keyPath: \RoomState.ownerInfo)
        modifyRoomState(value: 0, keyPath: \RoomState.maxCoGuestCount)
        context?.coHostManager.onLeaveRoom()
        context?.coGuestManager.onLeaveRoom()
        context?.mediaManager.onLeaveRoom()
        context?.userManager.onLeaveRoom()
        context?.viewManager.onLeaveRoom()
    }
    
    private func modifyRoomState<T>(value: T, keyPath: WritableKeyPath<RoomState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { roomState in
            roomState[keyPath: keyPath] = value
        }
    }
}
