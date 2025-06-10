//
//  LSRoomEngineService.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif
import ImSDK_Plus
import TUILiveResources

class LSRoomEngineService {
    let roomEngine: TUIRoomEngine
    let liveListManager: TUILiveListManager?
    let trtcCloud: TRTCCloud
    let beautyService: BeautyService
    
    init() {
        self.roomEngine = TUIRoomEngine.sharedInstance()
        self.liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager
        self.trtcCloud = roomEngine.getTRTCCloud()
        self.beautyService = BeautyService(roomEngine: roomEngine)
    }
    
    func addEngineObserver(_ engineObserver: TUIRoomObserver) {
        roomEngine.addObserver(engineObserver)
    }
    
    func removeEngineObserver(_ engineObserver: TUIRoomObserver) {
        roomEngine.removeObserver(engineObserver)
    }
    
    func addLiveListManagerObserver(_ observer: TUILiveListManagerObserver) {
        liveListManager?.addObserver(observer)
    }
    
    func removeLiveListManagerObserver(_ observer: TUILiveListManagerObserver) {
        liveListManager?.removeObserver(observer)
    }
}

// MARK: - RoomAPI
extension LSRoomEngineService {
    func syncLiveInfoToService(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveListManager = liveListManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveListManager error")
                continuation.resume(throwing: error)
                return
            }
            liveListManager.setLiveInfo(liveInfo, modifyFlag: modifyFlag) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
    
    func fetchLiveInfo(roomId: String) async throws -> TUILiveInfo {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveListManager = liveListManager else {
                let error = InternalError(code: ErrorLocalized.generalErrorCode, message: "get LiveListManager error")
                continuation.resume(throwing: error)
                return
            }
            liveListManager.getLiveInfo(roomId, onSuccess: { liveInfo in
                continuation.resume(returning: liveInfo)
            }, onError: { code, msg in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: msg))
            })
        }
    }
}

// MARK: - UserAPI
extension LSRoomEngineService {
    func getSelfInfo() -> TUILoginUserInfo {
        return TUIRoomEngine.getSelfInfo()
    }
    
    func getUserList() async throws -> [TUIUserInfo] {
        var allUsers: [TUIUserInfo] = []
        var nextSequence = 0
        while true {
            try await withCheckedThrowingContinuation {  [weak self] continuation in
                self?.roomEngine.getUserList(nextSequence: nextSequence) { userInfo, cursor in
                    allUsers.append(contentsOf: userInfo)
                    if cursor == 0 {
                        continuation.resume()
                    } else {
                        nextSequence = cursor
                        continuation.resume()
                    }
                } onError: { code, message in
                    continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
                }
            }
            if nextSequence == 0 {
                break
            }
        }
        return allUsers
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getUserInfo(userId) { userInfo in
                guard let user = userInfo else {
                    let error = InternalError(error: LiveError.userNotExist, message: LiveError.userNotExist.description)
                    continuation.resume(throwing: error)
                    return
                }
                continuation.resume(returning: user)
            } onError: { code, message in
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
    
    func muteAllRemoteAudio(isMute: Bool) {
        trtcCloud.muteAllRemoteAudio(isMute)
    }
    
    func disableSendingMessageByAdmin(userId: String, isDisable: Bool) async throws {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.disableSendingMessageByAdmin(userId: userId, isDisable: isDisable) {
                continuation.resume()
            } onError: { code, message in
                LiveKitLog.error("\(#file)","\(#line)",
                                 "disableSendingMessageByAdmin failed, error:\(code), message:\(String(describing: message))")
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
    
    func kickRemoteUserOutOfRoom(userId: String) async throws {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.kickRemoteUserOutOfRoom(userId) {
                continuation.resume()
            } onError: { code, message in
                LiveKitLog.error("\(#file)","\(#line)",
                                 "kickRemoteUserOutOfRoom failed, error:\(code), message:\(String(describing: message))")
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
}

// MARK: - MediaAPI
extension LSRoomEngineService {
    func setLocalVideoView(view: UIView) {
        roomEngine.setLocalVideoView(view: view)
    }
    
    func enableGravitySensor(enable: Bool) {
        roomEngine.enableGravitySensor(enable: enable)
    }
    
    func setVideoResolutionMode(_ resolutionMode: TUIResolutionMode) {
        roomEngine.setVideoResolutionMode(streamType: .cameraStream, resolutionMode: resolutionMode)
    }
    
    func updateVideoQuality(_ quality: TUIVideoQuality) {
        roomEngine.updateVideoQuality(quality)
    }
    
    func setBeautyStyle(_ style: TXBeautyStyle) {
        trtcCloud.getBeautyManager().setBeautyStyle(style)
    }
    
    func updateVideoQualityEx(streamType: TUIVideoStreamType, params: TUIRoomVideoEncoderParams) {
        roomEngine.updateVideoQualityEx(streamType: streamType, params: params)
    }
}

// MARK: - SeatAPI
extension LSRoomEngineService {
    func getSeatList() async throws -> [TUISeatInfo] {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getSeatList { seatList in
                continuation.resume(returning: seatList)
            } onError: { code, message in
                let error = InternalError(code: code.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func getSeatApplicationList() async throws -> [TUIRequest] {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getSeatApplicationList")
            roomEngine.getSeatApplicationList { requests in
                continuation.resume(returning: requests)
            } onError: { code, message in
                let error = InternalError(code: code.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func lockSeatByAdmin(seatIndex: Int, lockParams: TUISeatLockParams) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.lockSeatByAdmin(seatIndex, lockMode: lockParams) {
                continuation.resume()
            } onError: { code, message in
                LiveKitLog.error("\(#file)","\(#line)",
                                 "lockSeatByAdmin failed, error:\(code), message:\(String(describing: message))")
                continuation.resume(throwing: InternalError(code: code.rawValue, message: message))
            }
        }
    }
}
