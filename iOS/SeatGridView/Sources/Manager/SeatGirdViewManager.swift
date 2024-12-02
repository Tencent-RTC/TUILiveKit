//
//  VoiceRoomManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/17.
//

import Foundation
import Combine
import RTCRoomEngine
import RTCCommon

class SeatGridViewManager {
    class Context {
        
        let service = SeatGridViewService()
        let observers: SGSeatGridObserverList = SGSeatGridObserverList()
        
        private(set) lazy var roomManager = SGRoomManager(context: self)
        private(set) lazy var userManager = SGUserManager(context: self)
        private(set) lazy var mediaManager = SGMediaManager(context: self)
        private(set) lazy var seatManager = SGSeatManager(context: self)
        
        private(set) lazy var viewManager = SGViewManager(context: self)
        
        private lazy var engineObserver = SGRoomEngineObserver(context: self)
        
        init() {
            service.addRoomEngineObserver(engineObserver)
        }
        
        deinit {
            service.removeRoomEngineObserver(engineObserver)
        }
        
    }
    
    private let context = Context()
    
    func addObserver(_ observer: SeatGridViewObserver) {
        context.observers.addObserver(observer)
    }
    
    func removerObserver(_ observer: SeatGridViewObserver) {
        context.observers.removeObserver(observer)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

// MARK: - Room API
extension SeatGridViewManager {
    var roomState: SGRoomState {
        context.roomManager.roomState
    }
    
    func create(roomInfo: TUIRoomInfo) async throws -> TUIRoomInfo {
        return try await context.roomManager.create(roomInfo: roomInfo)
    }
    
    func destroy() async throws {
        return try await context.roomManager.destroy()
    }
    
    func enter(roomId: String) async throws -> TUIRoomInfo {
        return try await context.roomManager.enter(roomId: roomId)
    }
    
    func leave() async throws {
        return try await context.roomManager.exitRoom()
    }
    
    func update(seatMode: TUISeatMode) async throws {
        return try await context.roomManager.updateRoomSeatMode(seatMode: seatMode)
    }
}

// MARK: - User API
extension SeatGridViewManager {
    var userState: SGUserState {
        context.userManager.userState
    }
    
    func subscribeUserState<Value>(_ selector: StateSelector<SGUserState, Value>) -> AnyPublisher<Value, Never> {
        return context.userManager.observerState.subscribe(selector)
    }
    
    func refreshSelfInfo() {
        context.userManager.refreshSelfInfo()
    }
}

// MARK: - Media API
extension SeatGridViewManager {
    var mediaState: SGMediaState {
        context.mediaManager.mediaState
    }
    
    func startMicrophone() async throws {
        try await context.mediaManager.startMicrophone()
    }
    
    func stopMicrophone() {
        context.mediaManager.stopMicrophone()
    }
    
    func muteMicrophone() {
        context.mediaManager.muteMicrophone()
    }
    
    func unmuteMicrophone() async throws {
        try await context.mediaManager.unmuteMicrophone()
    }
}

// MARK: - Seat API
extension SeatGridViewManager {
    var seatState: SGSeatState {
        context.seatManager.seatState
    }
    
    func subscribeSeatState<Value>(_ selector: StateSelector<SGSeatState, Value>) -> AnyPublisher<Value, Never> {
        return context.seatManager.observerState.subscribe(selector)
    }
    
    func takeSeat(index: Int, timeout: Int) async throws -> SGTakeSeatResultWithUser {
       return try await context.seatManager.takeSeat(index: index, timeout: timeout)
    }
    
    func moveToSeat(index: Int) async throws {
        try await context.seatManager.moveToSeat(index: index)
    }
    
    func leaveSeat() async throws {
        try await context.seatManager.leaveSeat()
    }
    
    func lockSeat(index: Int, lockMode: TUISeatLockParams) async throws {
        try await context.seatManager.lockSeat(index: index, lockMode: lockMode)
    }
    
    func takeUserOnSeatByAdmin(index: Int,
                               userId: String,
                               timeout: Int) async throws -> SGTakeSeatResultWithUser {
        return try await context.seatManager.takeUserOnSeatByAdmin(index: index, userId: userId, timeout: timeout)
    }
    
    func kickUserOffSeatByAdmin(userId: String) async throws {
        return try await context.seatManager.kickUserOffSeatByAdmin(userId: userId)
    }
    
    func refreshSeatList() async throws {
        try await context.seatManager.refreshSeatList()
    }
    
    func responseRemoteRequest(userId: String, agree: Bool) async throws {
        try await context.seatManager.responseRemoteRequest(userId: userId, agree: agree)
    }
    
    func cancelRequest(userId: String) async throws {
        try await context.seatManager.cancelRequest(userId: userId)
    }
}

// MARK: View Layout API
extension SeatGridViewManager {
    func subscribeViewState<Value>(_ selector: StateSelector<SGViewState, Value>) -> AnyPublisher<Value, Never> {
        return context.viewManager.observerState.subscribe(selector)
    }
    
    func setLayoutMode(layoutMode: SGLayoutMode, layoutConfig: SGSeatViewLayoutConfig?) {
        context.viewManager.setLayoutMode(layoutMode: layoutMode, layoutConfig: layoutConfig)
    }
}

// MARK: - Observers notify.
extension SeatGridViewManager {
    func notifyObserverEvent(notifyAction: @escaping (_ observer: SeatGridViewObserver) -> Void) {
        Task {
            await context.observers.notifyObservers(callback: notifyAction)
        }
    }
}
