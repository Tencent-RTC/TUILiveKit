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
    
    func fetchUserInfo(_ userId: String) async throws -> VRUser {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.getUserInfo(userId) { userInfo in
                if let user = userInfo {
                    continuation.resume(returning: VRUser(userInfo: user))
                } else {
                    let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                    continuation.resume(throwing: error)
                }
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func fetchUserList() async throws -> [VRUser] {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            roomEngine.getUserList(nextSequence: 0) { [weak self] userInfoList, sequence in
                guard let self = self else { return }
                let userList = userInfoList.map { userInfo in
                    return VRUser(userInfo: userInfo)
                }
                continuation.resume(returning: userList)
            } onError: { error, message in
                let error = InternalError(error: error, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func followUser(userId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "followUser[userId:\(userId)]")
            imManager.followUser([userId]) { result in
                continuation.resume()
            } fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func unfollowUser(userId: String) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "unfollowUser[userId:\(userId)]")
            imManager.unfollowUser([userId]) { result in
                continuation.resume()
            } fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func checkFollowType(userId: String) async throws -> V2TIMFollowType {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self, let imManager = self.imManager else { return }
            imManager.checkFollowType([userId], succ: { result in
                guard let followType = result?.first?.followType else { return }
                continuation.resume(returning: followType)
            }, fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                continuation.resume(throwing: error)
            })
        }
    }
    
    func getSelfInfo() -> TUILoginUserInfo {
        return TUIRoomEngine.getSelfInfo()
    }
}
