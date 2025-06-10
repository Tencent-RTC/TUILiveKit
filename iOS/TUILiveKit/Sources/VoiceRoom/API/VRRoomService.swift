//
//  VRRoomService.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/8.
//

import RTCRoomEngine
import Combine
import TUILiveResources

// TODO: - Dependency management, needs to consolidate RoomEngine instances.
class VRRoomService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func fetchRoomInfo() async throws -> TUIRoomInfo {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            self.roomEngine.fetchRoomInfo { roomInfo in
                guard let roomInfo = roomInfo else {
                    let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "fetch room info fail.")
                    continuation.resume(throwing: error)
                    return
                }
                continuation.resume(returning: roomInfo)
            } onError: { err, message in
                let error = InternalError(code: err.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
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
