//
//  SGRoomManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//

import Foundation
import RTCRoomEngine


class SGRoomManager {
    private(set) var roomState: SGRoomState = SGRoomState()
   
    private typealias Context = SeatGridViewManager.Context
    private weak var context: Context?
    private let service: SeatGridViewService
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func create(roomInfo: TUIRoomInfo) async throws -> TUIRoomInfo {
        frameworkDataReport()
        roomInfo.roomType = .live
        roomInfo.isSeatEnabled = true
        return try await startLive(roomInfo: roomInfo)
    }
    
    func destroy() async throws {
        do {
            try await service.destroyRoom()
            refreshRoomState()
        } catch let SeatGridViewError.error(code, message){
            VRLog.error("\(#file)","\(#line)","destroy:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    
    func enter(roomId: String) async throws -> TUIRoomInfo {
        do {
            let roomInfo = try await self.service.enterRoom(roomId: roomId)
            update(roomInfo: roomInfo)
            frameworkDataReport()
            async let _ = try await context?.seatManager.onEnterRoom(roomInfo: roomInfo)
            return roomInfo
        } catch let SeatGridViewError.error(code, message){
            VRLog.error("\(#file)","\(#line)","enter:[roomId:\(roomId),code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } 
    }
    
    func exitRoom() async throws {
        do {
            try await service.exitRoom()
            refreshRoomState()
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func updateRoomSeatMode(seatMode: TUISeatMode) async throws {
        do {
            try await service.updateRoomSeatByModeByAdmin(seatMode: seatMode)
            modifyRoomState(value: seatMode, keyPath: \SGRoomState.seatMode)
        } catch let SeatGridViewError.error(code, message){
            VRLog.error("\(#file)","\(#line)","updateRoomSeatMode:[seatMode:\(seatMode),code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    private func startLive(roomInfo: TUIRoomInfo) async throws -> TUIRoomInfo {
        do {
            try await self.service.createRoom(roomInfo: roomInfo)
            let roomInfo = try await enter(roomId: roomInfo.roomId)
            // TODO: UserManager add OnEnterRoom
//            self.state.userState.selfInfo.userRole = .roomOwner
            return roomInfo
        } catch let SeatGridViewError.error(code, message){
            VRLog.error("\(#file)","\(#line)","create:[roomId:\(roomInfo.roomId),code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        }
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

// MARK: - Update State
extension SGRoomManager {
    func update(roomInfo: TUIRoomInfo) {
        roomState.ownerId = roomInfo.ownerId
        roomState.seatMode = roomInfo.seatMode
        roomState.maxSeatCount = roomInfo.maxSeatCount
        roomState.ownerName = roomInfo.ownerName
        roomState.ownerAvatar = roomInfo.ownerAvatarUrl
    }
    
    func modifyRoomState<T>(value: T, keyPath: WritableKeyPath<SGRoomState, T>) {
        roomState[keyPath: keyPath] = value
    }
    
    func refreshRoomState() {
        roomState = SGRoomState()
    }
}

// MARK: - DataReport
extension SGRoomManager {
    private func frameworkDataReport() {
        let apiParams: [String : Any] = [
            "api": "setFramework",
            "params": [
                "framework": SGConstants.DataReport.kDataReportFramework,
                "component": SGConstants.DataReport.kDataReportComponent,
                "language": SGConstants.DataReport.kDataReportLanguage,
            ],
          ]
        
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: apiParams, options: .prettyPrinted)
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                print(jsonString)
                TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString)
            } else {
                print("Error converting JSON data to string")
            }
        } catch {
            print("Error converting dictionary to JSON: \(error.localizedDescription)")
        }
    }
}
