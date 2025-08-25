//
//  VRRoomService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/8.
//

import RTCRoomEngine
import Combine

// TODO: - Dependency management, needs to consolidate RoomEngine instances.
class VRRoomService: BaseServiceProtocol {
    let liveGiftManager: TUILiveGiftManager?
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
        self.liveGiftManager = roomEngine.getExtension(extensionType: .liveGiftManager) as? TUILiveGiftManager
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func fetchRoomOwnerInfo(ownerId: String) async throws -> TUIUserInfo {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.getUserInfo(ownerId) { userInfo in
                if let user = userInfo {
                    continuation.resume(returning: user)
                } else {
                    let error = InternalError(error: LiveError.userNotExist, message: LiveError.userNotExist.description)
                    continuation.resume(throwing: error)
                }
                
            } onError: { error, message in
                let error = InternalError(code: error.rawValue, message: message)
                continuation.resume(throwing: error)
            }

        }
    }
    
    func fetchLiveInfo(roomId: String) async throws -> TUILiveInfo {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveListManager error")
                continuation.resume(throwing: error)
                return
            }
            liveListManager.getLiveInfo(roomId) { liveInfo in
                continuation.resume(returning: liveInfo)
            } onError: { error, message in
                let error = InternalError(code: error.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func fetchGiftCount(roomId: String) async throws -> (totalGiftsSent: UInt,  totalGiftCoins: UInt,  totalUniqueGiftSenders: UInt) {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveGiftManager = liveGiftManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveGiftManager error")
                continuation.resume(throwing: error)
                return
            }
            
            liveGiftManager.getGiftCountByAnchor(roomId: roomId) { totalGiftsSent, totalGiftCoins, totalUniqueGiftSenders in
                continuation.resume(returning: (totalGiftsSent, totalGiftCoins, totalUniqueGiftSenders))
            } onError: { code, msg in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: msg))
            }
        }
    }
    
    func fetchLikeCount(roomId: String) async throws -> UInt {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveGiftManager = liveGiftManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveGiftManager error")
                continuation.resume(throwing: error)
                return
            }
            
            liveGiftManager.getLikesCount(roomId: roomId) { totalLikesReceived in
                continuation.resume(returning: totalLikesReceived)
            } onError: { code, msg in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: msg))
            }
        }
    }
    
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveListManager error")
                continuation.resume(throwing: error)
                return
            }
            LiveKitLog.info("\(#file)", "\(#line)","setLiveInfo: [modifyFlag:\(modifyFlag.rawValue)]")
            liveListManager.setLiveInfo(liveInfo, modifyFlag: modifyFlag) {
                continuation.resume()
            } onError: { error, message in
                let error = InternalError(code: error.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func setRoomSeatModeByAdmin(_ mode: TUISeatMode) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)","setRoomSeatModeByAdmin: [mode:\(mode.rawValue)]")
            self.roomEngine.updateRoomSeatModeByAdmin(mode) {
                continuation.resume()
            } onError: { err, message in
                let error = InternalError(code: err.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
}
