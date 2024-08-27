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
    var roomEngine: TUIRoomEngine
    private let imManager = V2TIMManager.sharedInstance()
    
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }

    func fetchUserInfo(_ userId: String) -> AnyPublisher<User, InternalError> {
        return Future<User, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getUserInfo[userId:\(userId)]")
            roomEngine.getUserInfo(userId) { userInfo in
                LiveKitLog.info("\(#file)", "\(#line)", "getUserInfo[onSuccess]")
                if let user = userInfo {
                    promise(.success(User(userInfo: user)))
                } else {
                    let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                    promise(.failure(error))
                }
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getUserInfo[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func fetchUserList() -> AnyPublisher<[User], InternalError> {
        return Future<[User], InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getUserList[nextSequence:0]")
            roomEngine.getUserList(nextSequence: 0) { [weak self] userInfoList, sequence in
                guard let self = self else { return }
                LiveKitLog.info("\(#file)", "\(#line)", "getUserList[onSuccess]")
                let userList = userInfoList.map { userInfo in
                    return User(userInfo: userInfo)
                }
                promise(.success(userList))
            } onError: { error, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getUserList[onError:[error:\(error) message:\(message)]]")
                let error = InternalError(error: error, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func followUser(userId: String) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "followUser[userId:\(userId)]")
            self.imManager?.followUser([userId]) { result in
                LiveKitLog.info("\(#file)", "\(#line)", "followUser[onSuccess]")
                promise(.success(true))
            } fail: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "followUser[onError:[error:\(err) ,message:\(String(describing: message))]]")
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func unfollowUser(userId: String) -> AnyPublisher<Bool, InternalError> {
        return Future<Bool, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "unfollowUser[userId:\(userId)]")
            imManager.unfollowUser([userId]) { result in
                LiveKitLog.info("\(#file)", "\(#line)", "unfollowUser[onSuccess]")
                promise(.success(true))
            } fail: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "unfollowUser[onError:[error:\(err) ,message:\(String(describing: message))]]")
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func checkFollowType(userId: String) -> AnyPublisher<V2TIMFollowType, InternalError> {
        return Future<V2TIMFollowType, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "checkFollowType[userId:\(userId)]")
            imManager.checkFollowType([userId], succ: { result in
                LiveKitLog.info("\(#file)", "\(#line)", "checkFollowType[onSuccess]")
                guard let followType = result?.first?.followType else { return }
                promise(.success(followType))
            }, fail: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "checkFollowType[onError:[error:\(err) ,message:\(String(describing: message))]]")
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            })
        }
        .eraseToAnyPublisher()
    }
    
    func fetchFollowersCount(userId: String) -> AnyPublisher<Int, InternalError> {
        return Future<Int, InternalError> { [weak self] promise in
            guard let self = self, let imManager = self.imManager else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getUserFollowInfo[userId:\(userId)]")
            imManager.getUserFollowInfo([userId], succ: { followInfo in
                LiveKitLog.info("\(#file)", "\(#line)", "getUserFollowInfo[onSuccess]")
                let followersCount = Int(followInfo?.first?.followersCount ?? 0)
                promise(.success(followersCount))
            }, fail: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getUserFollowInfo[onError:[error:\(err) ,message:\(String(describing: message))]]")
                let error = InternalError(error: TIMError.invalidUserId, message: TIMError.invalidUserId.description)
                promise(.failure(error))
            })
        }.eraseToAnyPublisher()
    }
}

extension UserService {
    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, view: UIView?) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", 
                            "setRemoteVideoView[userId:\(userId),streamType:\(streamType.rawValue),view:\(String(describing: view))]")
            roomEngine.setRemoteVideoView(userId: userId, streamType: streamType, view: view)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
    
    func startPlayRemoteVideo(userId: String,
                              streamType: TUIVideoStreamType)
    -> AnyPublisher<PlayRemoteUserVideoStatus, InternalError> {
        return Future<PlayRemoteUserVideoStatus, InternalError> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)",
                            "startPlayRemoteVideo[userId:\(userId),streamType:\(streamType.rawValue)]")
            roomEngine.startPlayRemoteVideo(userId: userId, streamType: streamType, onPlaying: { userId in
                LiveKitLog.info("\(#file)", "\(#line)",
                                "startPlayRemoteVideo[onPlaying:[userId:\(userId)]]")
                promise(.success(.onPlaying(userId: userId)))
            }, onLoading: { userId in
                LiveKitLog.info("\(#file)", "\(#line)",
                                "startPlayRemoteVideo[onLoading:[userId:\(userId)]]")
                promise(.success(.onLoading(userId: userId)))
            }, onError: { userId, err, message in
                LiveKitLog.error("\(#file)", "\(#line)", 
                                "startPlayRemoteVideo[onError:[error:\(err) ,message:\(String(describing: message)) ,userId:\(userId)]]")
                let error = InternalError(error: err, message: message)
                promise(.failure(error))
            })
        }
        .eraseToAnyPublisher()
    }
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType) -> AnyPublisher<Void, Never> {
        return Future<Void, Never> { [weak self] promise in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)",
                            "stopPlayRemoteVideo[userId:\(userId),streamType:\(streamType.rawValue)]")
            roomEngine.stopPlayRemoteVideo(userId: userId, streamType: streamType)
            promise(.success(()))
        }
        .eraseToAnyPublisher()
    }
}
