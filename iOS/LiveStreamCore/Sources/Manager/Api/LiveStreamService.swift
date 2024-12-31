//
//  LiveStreamService.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import Foundation
import ImSDK_Plus
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

enum LiveStreamCoreError: Error {
    case error(code: TUIError, message: String)
    case playVideoError(userId: String, code: TUIError, message: String)
    case seatError(userId: String, code: TUIError, message: String)
}

enum TakeSeatResult {
    case accepted(userId: String)
    case rejected(userId: String)
    case timeout(userId: String)
    case cancel(userId: String)
}

class LiveStreamService: LiveStream {
    let roomEngine: TUIRoomEngine
    let trtcCloud: TRTCCloud
    let connectionManager: TUILiveConnectionManager
    let battleManager: TUILiveBattleManager
    let layoutManager: TUILiveLayoutManager?
    
    init() {
        roomEngine = TUIRoomEngine.sharedInstance()
        trtcCloud = roomEngine.getTRTCCloud()
        connectionManager = roomEngine.getLiveConnectionManager()
        battleManager = roomEngine.getLiveBattleManager()
        layoutManager = roomEngine.getExtension(extensionType: .liveLayoutManager) as? TUILiveLayoutManager
    }

    func destroy() {
        destroyEngine()
    }
    
    func addRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.addObserver(observer)
    }
    
    func removeRoomEngineObserver(_ observer: any TUIRoomObserver) {
        roomEngine.removeObserver(observer)
    }
    
    func addLiveConnectionManagerObserver(_ observer: any TUILiveConnectionObserver) {
        connectionManager.addObserver(observer)
    }
    
    func removeLiveConnectionManagerObserver(_ observer: any TUILiveConnectionObserver) {
        connectionManager.removeObserver(observer)
    }
    
    func addLiveBattleManagerObserver(_ observer: any TUILiveBattleObserver) {
        battleManager.addObserver(observer)
    }
    
    func removeLiveBattleManagerObserver(_ observer: any TUILiveBattleObserver) {
        battleManager.removeObserver(observer)
    }
    
    func addImObserver(_ observer: V2TIMSDKListener) {
        V2TIMManager.sharedInstance().add(observer)
    }
    
    func removeImObserver(_ observer: V2TIMSDKListener) {
        V2TIMManager.sharedInstance().remove(observer)
    }
    
    func addLiveLayoutManagerObserver(_ observer: any TUILiveLayoutObserver) {
        guard let layoutManager = layoutManager else { return }
        layoutManager.addObserver(observer)
    }
    
    func removeLiveLayoutManagerObserver(_ observer: any TUILiveLayoutObserver) {
        guard let layoutManager = layoutManager else { return }
        layoutManager.removeObserver(observer)
    }
    
    func createRoom(roomInfo: TUIRoomInfo) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.createRoom(roomInfo) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func enterRoom(roomId: String) async throws -> TUIRoomInfo {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.enterRoom(roomId, roomType: .live) { roomInfo in
                guard let info = roomInfo else {
                    continuation.resume(throwing: LiveStreamCoreError.error(code: .roomIdInvalid,
                                                                            message: TUIError.roomIdInvalid.lcDescription))
                    return
                }
                continuation.resume(returning: info)
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func exitRoom() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.exitRoom(syncWaiting: true) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func destroyRoom() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.destroyRoom {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func takeSeat(seatIndex: Int,
                  timeout: TimeInterval,
                  requestCallback: SentRequestResultClosure? = nil) async throws -> TakeSeatResult {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            let request = roomEngine.takeSeat(seatIndex, timeout: TimeInterval(timeout)) { requestId, userId in
                continuation.resume(returning: .accepted(userId: userId))
            } onRejected: { requestId, userId, message in
                continuation.resume(returning: .rejected(userId: userId))
            } onCancelled: { requestId, userId in
                continuation.resume(returning: .cancel(userId: userId))
            } onTimeout: { requestId, userId in
                continuation.resume(returning: .timeout(userId: userId))
            } onError: { requestId, userId, code, message in
                continuation.resume(throwing: LiveStreamCoreError.seatError(userId: userId, code: code, message: message))
            }
            requestCallback?(request)
        }
    }
    
    func takeUserOnSeatByAdmin(seatIndex: Int,
                               userId: String,
                               timeout: TimeInterval,
                               requestCallback: SentRequestResultClosure? = nil) async throws -> TakeSeatResult {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            let request = roomEngine.takeUserOnSeatByAdmin(seatIndex, userId: userId, timeout: timeout) { requestId, userId in
                continuation.resume(returning: .accepted(userId: userId))
            } onRejected: { requestId, userId, message in
                continuation.resume(returning: .rejected(userId: userId))
            } onCancelled: { requestId, userId in
                continuation.resume(returning: .cancel(userId: userId))
            } onTimeout: { requestId, userId in
                continuation.resume(returning: .timeout(userId: userId))
            } onError: { requestId, userId, code, message in
                continuation.resume(throwing: LiveStreamCoreError.seatError(userId: userId, code: code, message: message))
            }
            requestCallback?(request)
        }
    }
    
    func leaveSeat() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.leaveSeat() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func lockSeat(seatIndex: Int, lockMode: TUISeatLockParams) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.lockSeatByAdmin(seatIndex, lockMode: lockMode) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func getSeatList() async throws -> [TUISeatInfo] {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getSeatList() { list in
                continuation.resume(returning: list)
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func getSeatApplicationList() async throws -> [TUIRequest] {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getSeatApplicationList() { list in
                continuation.resume(returning: list)
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func responseRemoteRequest(requestId: String, agree: Bool) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.responseRemoteRequest(requestId, agree: agree) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func cancelRequest(requestId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.cancelRequest(requestId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func kickUserOffSeatByAdmin(seatIndex: Int, userId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.kickUserOffSeatByAdmin(seatIndex, userId: userId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.getUserInfo(userId) { userInfo in
                guard let user = userInfo else {
                    continuation.resume(throwing: LiveStreamCoreError.error(code: TUIError.userNotExist,
                                                                            message: TUIError.userNotExist.lcDescription))
                    return
                }
                continuation.resume(returning: user)
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func openLocalMicrophone() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.openLocalMicrophone(.default) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func closeLocalMicrophone() {
        roomEngine.closeLocalMicrophone()
    }
    
    func openLocalCamera(isFront: Bool, quality: TUIVideoQuality) async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.openLocalCamera(isFront: isFront, quality: quality) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func closeLocalCamera() {
        roomEngine.closeLocalCamera()
    }
    
    
    func switchCamera(isFrontCamera: Bool) {
        roomEngine.getMediaDeviceManager().switchCamera(isFrontCamera)
    }

    func updateVideoQualityEx(streamType: TUIVideoStreamType, params: TUIRoomVideoEncoderParams) {
        roomEngine.updateVideoQualityEx(streamType: streamType, params: params)
    }
    
    func setLocalVideoView(_ view: UIView?) {
        roomEngine.setLocalVideoView(view: view)
    }
     
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, videoView: UIView) {
        roomEngine.setRemoteVideoView(userId: userId, streamType: streamType, view: videoView)
    }
    
    func startPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType, onLoading: @escaping TUIPlayOnLoadingBlock) async throws -> String {
        return try await withCheckedThrowingContinuation { continuation in
            roomEngine.startPlayRemoteVideo(userId: userId, streamType: streamType) { userId in
                continuation.resume(returning: userId)
            } onLoading: { userId in
                onLoading(userId)
            } onError: { userId, code, message in
                continuation.resume(throwing: LiveStreamCoreError.playVideoError(userId: userId, code: code, message: message))
            }
        }
    }
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) {
        roomEngine.stopPlayRemoteVideo(userId: userId, streamType: streamType)
    }
    
    func muteLocalAudio() {
        roomEngine.muteLocalAudio()
    }
    
    func unMuteLocalAudio() async throws {
        try await withCheckedThrowingContinuation { continuation in
            roomEngine.unmuteLocalAudio() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func enableGravitySensor(enable: Bool) {
        roomEngine.enableGravitySensor(enable: enable)
    }
    
    func setVideoResolutionMode(_ resolutionMode: TUIResolutionMode) {
        roomEngine.setVideoResolutionMode(streamType: .cameraStream, resolutionMode: resolutionMode)
    }
    
    func setBeautyStyle(_ style: TXBeautyStyle) {
        trtcCloud.getBeautyManager().setBeautyStyle(style)
    }
    
    func callExperimentalAPI(jsonStr: String) {
        TUIRoomEngine.callExperimentalAPI(jsonStr: jsonStr)
    }
    
    func getTRTCCloud() -> TRTCCloud {
        return trtcCloud
    }
    
    func requestConnection(roomIdList: [String], timeout: Int, extensionInfo: String) async throws -> [String: TUIConnectionCode] {
        return try await withCheckedThrowingContinuation { continuation in
            connectionManager.requestConnection(roomIdList: roomIdList, timeout: TimeInterval(timeout), extensionInfo: extensionInfo) { result in
                var connectionResult: [String: TUIConnectionCode] = [:]
                result.forEach { (key: String, value: NSNumber) in
                    connectionResult[key] = TUIConnectionCode(rawValue: value.intValue) ?? .unknown
                }
                continuation.resume(returning: connectionResult)
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func acceptConnection(roomId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            connectionManager.acceptConnection(roomId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func rejectConnection(roomId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            connectionManager.rejectConnection(roomId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func disconnect() async throws {
        try await withCheckedThrowingContinuation { continuation in
            connectionManager.disconnect() {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func cancelConnectionRequest(list: [String]) async throws {
        try await withCheckedThrowingContinuation { continuation in
            connectionManager.cancelConnectionRequest(roomIdList: list) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func requestBattle(config: TUIBattleConfig, userIdList: [String], timeout: TimeInterval) async throws -> (TUIBattleInfo, [String: NSNumber]) {
        return try await withCheckedThrowingContinuation { continuation in
            battleManager.requestBattle(config: config, userIdList: userIdList, timeout: timeout) { battleInfo, resultMap in
                continuation.resume(returning: (battleInfo, resultMap))
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func cancelBattle(battleId: String, userIdList: [String]) async throws {
        try await withCheckedThrowingContinuation { continuation in
            battleManager.cancelBattleRequest(battleId: battleId, userIdList: userIdList) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func acceptBattle(battleId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            battleManager.acceptBattle(battleId: battleId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func rejectBattle(battleId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            battleManager.rejectBattle(battleId: battleId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
    
    func exitBattle(battleId: String) async throws {
        try await withCheckedThrowingContinuation { continuation in
            battleManager.exitBattle(battleId: battleId) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: LiveStreamCoreError.error(code: code, message: message))
            }
        }
    }
}

// MARK: - Private
extension LiveStreamService {
    private func destroyEngine() {
        LiveStreamLog.info("\(#file)", "\(#line)", "destroyEngine:[]")
        
        let jsonObject: [String: Any] = ["api": "destroySubRoom"]
        
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: jsonObject, options: [])
            if let jsonString = String(data: jsonData, encoding: .utf8) {
                TUIRoomEngine.callExperimentalAPI(jsonStr: jsonString)
            }
        } catch {
            fatalError("Failed to serialize JSON: \(error)")
        }
    }
}
