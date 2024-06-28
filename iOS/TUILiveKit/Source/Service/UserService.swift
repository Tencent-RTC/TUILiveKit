//
//  UserService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

import RTCRoomEngine
import Combine
import ImSDK_Plus

enum PlayRemoteUserVideoStatus {
    case onPlaying(userId: String)
    case onLoading(userId: String)
}


class UserService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine?
    private let imManager = V2TIMManager.sharedInstance()
    
    required init(roomEngine: TUIRoomEngine?) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }

    func fetchUserInfo(_ userId: String) -> AnyPublisher<User, InternalError> {
        return Future<User, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getUserInfo(userId) { userInfo in
                if let user = userInfo {
                    promise(.success(User(userInfo: user)))
                } else {
                    let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                    promise(.failure(error))
                }
            } onError: { err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func fetchUserList() -> AnyPublisher<[User], InternalError> {
        return Future<[User], InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.getUserList(nextSequence: 0) { [weak self] userInfoList, sequence in
                guard let self = self else { return }
                let userList = userInfoList.map { userInfo in
                    return User(userInfo: userInfo)
                }
                promise(.success(userList))
            } onError: { error, message in
                let error = InternalError(error: error, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func followUser(userId: String) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.imManager?.followUser([userId]) { result in
                promise(.success(true))
            } fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func unfollowUser(userId: String) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            imManager.unfollowUser([userId]) { result in
                promise(.success(true))
            } fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func checkFollowType(userId: String) -> AnyPublisher<V2TIMFollowType, InternalError> {
        return Future<V2TIMFollowType, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            imManager.checkFollowType([userId], succ: { result in
                guard let followType = result?.first?.followType else { return }
                promise(.success(followType))
            }, fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            })
        }
        .eraseToAnyPublisher()
    }
    
    func fetchFollowersCount(userId: String) -> AnyPublisher<Int, InternalError> {
        return Future<Int, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            imManager.getUserFollowInfo([userId], succ: { followInfo in
                let followersCount = Int(followInfo?.first?.followersCount ?? 0)
                promise(.success(followersCount))
            }, fail: { err, message in
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            })
        }.eraseToAnyPublisher()
    }
}

extension UserService {
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, view: UIView?) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.setRemoteVideoView(userId: userId, streamType: streamType, view: view)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func startPlayRemoteVideo(userId: String,
                              streamType: TUIVideoStreamType)
    -> AnyPublisher<PlayRemoteUserVideoStatus, InternalError> {
        return Future<PlayRemoteUserVideoStatus, InternalError> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.startPlayRemoteVideo(userId: userId, streamType: streamType, onPlaying: { userId in
                promise(.success(.onPlaying(userId: userId)))
            }, onLoading: { userId in
                promise(.success(.onLoading(userId: userId)))
            }, onError: { userId, err, message in
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            })
        }
        .eraseToAnyPublisher()
    }
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self, let roomEngine = self.roomEngine else { return }
            roomEngine.stopPlayRemoteVideo(userId: userId, streamType: streamType)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
