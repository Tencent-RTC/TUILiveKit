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
    
    func addBattleObserver(_ observer: TUILiveBattleObserver) {
        roomEngine.getLiveBattleManager().addObserver(observer)
    }
    
    func removeBattleObserver(_ observer: TUILiveBattleObserver) {
        roomEngine.getLiveBattleManager().removeObserver(observer)
    }
}

// MARK: - RoomAPI
extension LSRoomEngineService {
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let liveListManager = liveListManager else {
                let error = InternalError(error: TUIError.failed, message: TUIError.failed.description)
                continuation.resume(throwing: error)
                return
            }
            liveListManager.setLiveInfo(liveInfo, modifyFlag: modifyFlag) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: code, message: message))
            }
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
                    continuation.resume(throwing: InternalError(error: code, message: message))
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
                    let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                    continuation.resume(throwing: error)
                    return
                }
                continuation.resume(returning: user)
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: code, message: message))
            }
        }
    }
    
    func muteAllRemoteAudio(isMute: Bool) {
        trtcCloud.muteAllRemoteAudio(isMute)
    }
}

// MARK: - MediaAPI
extension LSRoomEngineService {
    func setLocalVideoView(view: UIView) {
        roomEngine.setLocalVideoView(view: view)
    }
    
    func openLocalCamera(isFront: Bool, quality: TUIVideoQuality) async throws {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.openLocalCamera(isFront: isFront, quality: quality) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: code, message: message))
            }
        }
    }
    
    func closeLocalCamera() {
        roomEngine.closeLocalCamera()
    }
    
    func switchCamera(frontCamera: Bool) {
        roomEngine.getMediaDeviceManager().switchCamera(frontCamera)
    }
    
    func setCameraMirror(enable: Bool) {
        let params = TRTCRenderParams()
        params.mirrorType = enable ? .enable : .disable
        trtcCloud.setLocalRenderParams(params)
        trtcCloud.setVideoEncoderMirror(enable)
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
}

// MARK: - SeatAPI
extension LSRoomEngineService {
    func getSeatList() async throws -> [TUISeatInfo] {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getSeatList { seatList in
                continuation.resume(returning: seatList)
            } onError: { code, message in
                let error = InternalError(error: code, message: message)
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
                let error = InternalError(error: code, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
}
