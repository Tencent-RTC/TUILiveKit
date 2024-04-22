//
//  UserService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

import RTCRoomEngine
import Combine


class UserService {
    
    @WeakLazyInjected var store: VoiceRoomStoreProvider?
    
    private let engine = TUIRoomEngine.sharedInstance()
    
    func getSelfInfo() -> User {
        let info = TUIRoomEngine.getSelfInfo()
        return User(loginInfo: info)
    }
    
    func fetchUserInfo(_ userId: String) -> AnyPublisher<User, InternalError> {
        return Future<User, InternalError> { [weak self] promise in
            guard let self = self else { return }
            self.engine.getUserInfo(userId) { userInfo in
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
            guard let self = self, let store = self.store else { return }
            self.engine.getUserList(nextSequence: 0) { userInfoList, sequence in
                let userList = userInfoList.filter { $0.userId != store.selectCurrent(RoomSelectors.getRoomOwnerId) }.map { userInfo in
                    return User(userInfo: userInfo)
                }
                promise(.success(userList))
            } onError: { error, message in
                let error = InternalError(error: error, message: message)
                promise(.failure(error))
            }
        }.eraseToAnyPublisher()
    }
    
    func fetchRoomOwnerInfo() -> AnyPublisher<User, InternalError> {
        return Future<User, InternalError> { [weak self] promise in
            guard let self = self else { return }
            guard let ownerId = store?.selectCurrent(RoomSelectors.getRoomOwnerId) else {
                let error = InternalError(error: TUIError.userNotExist, message: TUIError.userNotExist.description)
                promise(.failure(error))
                return
            }
            self.engine.getUserInfo(ownerId) { userInfo in
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
}
