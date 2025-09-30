//
//  AudienceService.swift
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

class AudienceService {
    let roomEngine: TUIRoomEngine
    let liveListManager: TUILiveListManager?
    let trtcCloud: TRTCCloud
    let layoutManager: TUILiveLayoutManager?

    
    init() {
        self.roomEngine = TUIRoomEngine.sharedInstance()
        self.liveListManager = roomEngine.getExtension(extensionType: .liveListManager) as? TUILiveListManager
        self.trtcCloud = roomEngine.getTRTCCloud()
        self.layoutManager = roomEngine.getExtension(extensionType: .liveLayoutManager) as? TUILiveLayoutManager
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
    
    func addLiveLayoutObserver(_ observer: TUILiveLayoutObserver) {
        layoutManager?.addObserver(observer)
    }
    
    func removeLiveLayoutObserver(_ observer: TUILiveLayoutObserver) {
        layoutManager?.removeObserver(observer)
    }
}

// MARK: - RoomAPI
extension AudienceService {
    
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
extension AudienceService {
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
}

// MARK: - MediaAPI
extension AudienceService {
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
    
    func switchPlaybackQuality(_ quality: TUIVideoQuality) {
        var jsonObject = [String: Any]()
        jsonObject["api"] = "switchPlaybackQuality"
        var params = [String: Any]()
        params["quality"] = quality.rawValue
        jsonObject["params"] = params
        guard let jsonData = try? JSONSerialization.data(withJSONObject: jsonObject, options: []),
              let jsonString = String(data: jsonData, encoding: .utf8) else {
            return
        }
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonString) { msg in
            LiveKitLog.info("\(#file)", "\(#line)", "switchPlaybackQuality msg: \(msg)")
        }
    }
    
    func getMultiPlaybackQuality(roomId: String, completion: (([TUIVideoQuality]) -> Void)? = nil) {
        var jsonObject = [String: Any]()
        jsonObject["api"] = "queryPlaybackQualityList"
        var params = [String: Any]()
        params["roomId"] = roomId
        jsonObject["params"] = params
        guard let jsonData = try? JSONSerialization.data(withJSONObject: jsonObject, options: []),
              let jsonString = String(data: jsonData, encoding: .utf8) else {
            return
        }
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonString) { [weak self] msg in
            guard let self = self else { return }
            self.decodeQualityJsonString(msg, completion: completion)
        }
    }
    
    private func decodeQualityJsonString(_ jsonString: String, completion: (([TUIVideoQuality]) -> Void)? = nil) {
        struct Response: Decodable {
            let code: Int
            let data: [Int32]
            let message: String
        }
        guard let jsonData = jsonString.data(using: .utf8),
              let response = try? JSONDecoder().decode(Response.self, from: jsonData) else {
            LiveKitLog.error("\(#file)", "\(#line)", "msg: \(jsonString)")
            return
        }
        var qualityList: [TUIVideoQuality] = []
        for quality in response.data {
            if quality == TUIVideoQuality.quality1080P.rawValue {
                qualityList.append(.quality1080P)
            }
            if quality == TUIVideoQuality.quality720P.rawValue {
                qualityList.append(.quality720P)
            }
            if quality == TUIVideoQuality.quality540P.rawValue {
                qualityList.append(.quality540P)
            }
            if quality == TUIVideoQuality.quality360P.rawValue {
                qualityList.append(.quality360P)
            }
        }
        completion?(qualityList)
    }
}

// MARK: - SeatAPI
extension AudienceService {
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
}
