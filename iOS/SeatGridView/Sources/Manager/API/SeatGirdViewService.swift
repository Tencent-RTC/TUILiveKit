//
//  SeatGridViewServiceProtocol.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/17.
//

import Foundation
import RTCRoomEngine

enum SeatGridViewError: Error {
    case error(code: Int, message: String)
    case seatErrorWithId(requestId: String, userId: String, code: Int, message: String)
    case seatErrorWithUser(user: TUIUserInfo, code: Int, message: String)
}

enum SGTakeSeatResultWithId {
    case accepted(requestId: String, userId: String)
    case rejected(requestId: String, userId: String)
    case timeout(requestId: String, userId: String)
    case cancel(requestId: String, userId: String)
}

class SeatGridViewService: SeatGridViewInterface {
    let roomEngine = TUIRoomEngine.sharedInstance()
    
    func addRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.addObserver(observer)
    }
    
    func removeRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.removeObserver(observer)
    }
    
    func createRoom(roomInfo: TUIRoomInfo) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.createRoom(roomInfo) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func destroyRoom() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.destroyRoom() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func enterRoom(roomId: String) async throws -> TUIRoomInfo {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.enterRoom(roomId, roomType: .live) { engineRoomInfo in
                guard let roomInfo = engineRoomInfo else {
                    continuation.resume(throwing: SeatGridViewError.error(code: TUIError.roomIdInvalid.rawValue,
                                                                          message: TUIError.roomIdInvalid.description))
                    return
                }
                continuation.resume(returning: roomInfo)
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func exitRoom() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.exitRoom(syncWaiting: true) {
                      continuation.resume()
                  } onError: { code, message in
                      continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func getLiveInfo(roomId: String) async throws -> TUILiveInfo {
        return try await withCheckedThrowingContinuation { continuation in
            guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                continuation.resume(throwing: SeatGridViewError.error(code: TUIError.failed.rawValue,
                                                                      message: TUIError.failed.description))
                return
            }
            liveListManager.getLiveInfo(roomId) { liveInfo in
                continuation.resume(returning: liveInfo)
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) async throws {
        return try await withCheckedThrowingContinuation { continuation in
            guard let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                continuation.resume(throwing: SeatGridViewError.error(code: TUIError.failed.rawValue,
                                                                      message: TUIError.failed.description))
                return
            }
            liveListManager.setLiveInfo(liveInfo, modifyFlag: modifyFlag) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func updateRoomSeatByModeByAdmin(seatMode: TUISeatMode) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.updateRoomSeatModeByAdmin(seatMode) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func lockSeat(index: Int, lockMode: TUISeatLockParams) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.lockSeatByAdmin(index, lockMode: lockMode) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func takeSeat(seatIndex: Int,timeout: TimeInterval, requestCallback: SentRequestResultClosure) async throws -> SGTakeSeatResultWithId {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            let request = roomEngine.takeSeat(seatIndex,
                                              timeout: TimeInterval(timeout)) { requestId, userId in
                continuation.resume(returning: SGTakeSeatResultWithId.accepted(requestId: requestId, userId: userId))
            } onRejected: { requestId, userId, message in
                continuation.resume(returning: SGTakeSeatResultWithId.rejected(requestId: requestId, userId: userId))
            } onCancelled: { requestId, userId in
                continuation.resume(returning: SGTakeSeatResultWithId.cancel(requestId: requestId, userId: userId))
            } onTimeout: { requestId, userId in
                continuation.resume(returning: SGTakeSeatResultWithId.timeout(requestId: requestId, userId: userId))
            } onError: { requestId, userId, code, message in
                continuation.resume(throwing: SeatGridViewError.seatErrorWithId(requestId: requestId,
                                                                                userId: userId,
                                                                                code: code.rawValue,
                                                                                message: message))
            }
            requestCallback(request)
        }
    }
    
    func takeUserOnSeatByAdmin(seatIndex: Int,
                               userId: String,
                               timeout: TimeInterval,
                               requestCallback: SentRequestResultClosure) async throws -> SGTakeSeatResultWithId {
        return try await withCheckedThrowingContinuation { continuation in
            let request = roomEngine.takeUserOnSeatByAdmin(seatIndex,
                                             userId: userId,
                                             timeout: TimeInterval(timeout)) { requestId, userId in
                continuation.resume(returning: .accepted(requestId: requestId, userId: userId))
            } onRejected: { requestId, userId, message in
                continuation.resume(returning: .rejected(requestId: requestId, userId: userId))
            } onCancelled: { requestId, userId in
                continuation.resume(returning: .cancel(requestId: requestId, userId: userId))
            } onTimeout: { requestId, userId in
                continuation.resume(returning: .timeout(requestId: requestId, userId: userId))
            } onError: { requestId, userId, code, message in
                continuation.resume(throwing: SeatGridViewError.seatErrorWithId(requestId: requestId,
                                                                          userId: userId,
                                                                          code: code.rawValue,
                                                                          message: message))
            }
            requestCallback(request)
        }
    }
    
    func leaveSeat() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.leaveSeat() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func moveSeat(index: Int) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.moveToSeat(targetSeatIndex: index) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func getSeatList() async throws -> [TUISeatInfo] {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.getSeatList { seatInfoList in
                continuation.resume(returning: seatInfoList)
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
        
    func responseRemoteRequest(requestId: String, agree: Bool) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.responseRemoteRequest(requestId, agree: agree) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func cancelRequest(requestId: String)  async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.cancelRequest(requestId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func kickUserOffSeatByAdmin(userId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.kickUserOffSeatByAdmin(-1, userId: userId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    
    func getUserList() async throws -> [TUIUserInfo] {
        var allUsers: [TUIUserInfo] = []
            var nextSequence = 0

            while true {
                try await withCheckedThrowingContinuation { continuation in
                    roomEngine.getUserList(nextSequence: nextSequence) { userInfo, cursor in
                        allUsers.append(contentsOf: userInfo)
                        if cursor == 0 {
                            continuation.resume()
                        } else {
                            nextSequence = cursor
                            continuation.resume()
                        }
                    } onError: { code, message in
                        continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
                    }
                }
              
                if nextSequence == 0 {
                    break
                }
            }
            return allUsers
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.getUserInfo(userId) { userInfo in
                guard let user = userInfo else {
                    continuation.resume(throwing: SeatGridViewError.error(code: TUIError.userNotExist.rawValue,
                                                                          message: TUIError.userNotExist.description))
                    return
                }
                continuation.resume(returning: user)
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func openLocalMicrophone() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.openLocalMicrophone(.speech) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func closeLocalMicrophone() {
        roomEngine.closeLocalMicrophone()
    }
    
    func muteLocalAudio() {
        roomEngine.muteLocalAudio()
    }
    
    func unmuteLocalAudio() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.unmuteLocalAudio() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: SeatGridViewError.error(code: code.rawValue, message: message))
            }
        }
    }
    
    func updateAudioQuality(quality: TUIAudioQuality) {
        roomEngine.updateAudioQuality(quality)
    }
    
    func getSelfInfo() -> TUILoginUserInfo {
        return TUIRoomEngine.getSelfInfo()
        
    }
    
    func callExperimentalAPI(jsonString: String) {
        TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}
