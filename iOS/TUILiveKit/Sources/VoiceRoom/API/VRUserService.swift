//
//  VRUserService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

import RTCRoomEngine
import Combine
import ImSDK_Plus

class VRUserService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    private let imManager = V2TIMManager.sharedInstance()
    
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func fetchUserInfo(_ userId: String) async throws -> TUIUserInfo {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.getUserInfo(userId) { userInfo in
                if let user = userInfo {
                    continuation.resume(returning: user)
                } else {
                    let error = InternalError(error: LiveError.userNotExist, message: LiveError.userNotExist.description)
                    continuation.resume(throwing: error)
                }
            } onError: { err, message in
                let error = InternalError(code: err.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func fetchUserList() async throws -> [TUIUserInfo] {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.getUserList(nextSequence: 0) { userInfoList, sequence in
                continuation.resume(returning: userInfoList)
            } onError: { error, message in
                let error = InternalError(code: error.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func followUser(userId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "followUser[userId:\(userId)]")
            imManager.followUser(userIDList: [userId]) { result in
                continuation.resume()
            } fail: { err, message in
                let error = InternalError(code: Int(err), message: message ?? "")
                continuation.resume(throwing: error)
            }
        }
    }
    
    func unfollowUser(userId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "unfollowUser[userId:\(userId)]")
            imManager.unfollowUser(userIDList: [userId]) { result in
                continuation.resume()
            } fail: { err, message in
                let error = InternalError(code: Int(err), message: message ?? "")
                continuation.resume(throwing: error)
            }
        }
    }
    
    func checkFollowType(userId: String) async throws -> V2TIMFollowType {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            imManager.checkFollowType(userIDList: [userId], succ: { result in
                guard let followType = result?.first?.followType else { return }
                continuation.resume(returning: followType)
            }, fail: { err, message in
                let error = InternalError(code: Int(err), message: message ?? "")
                continuation.resume(throwing: error)
            })
        }
    }
    
    func getSelfInfo() -> TUILoginUserInfo {
        return TUIRoomEngine.getSelfInfo()
    }
}
