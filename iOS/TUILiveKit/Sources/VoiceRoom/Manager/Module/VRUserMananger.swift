//
//  UserMananger.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import Foundation
import Combine
import RTCCommon
import ImSDK_Plus
import RTCRoomEngine
import TUILiveComponent

class VRUserManager: VRIMObserverInterface, VRRoomEngineObserverUserInterface {
    var state: VRUserState {
        observerState.state
    }
    
    private typealias Context = VoiceRoomManager.Context
    private let observerState = ObservableState<VRUserState>(initialState: VRUserState())
    private weak var context: Context?
    private weak var service: VRUserService? {
        context?.service.userService
    }
    private let toastSubject: PassthroughSubject<String, Never>
    required init(context: VoiceRoomManager.Context) {
        self.context = context
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { state in
            state.myFollowingUserList = []
            state.speakingUserList = []
            state.userList = []
        }
    }
}

extension VRUserManager {
    func subscribeState<Value>(_ selector: StateSelector<VRUserState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo]) {
        guard let context = context else { return }
        var linkStatus: LinkStatus = .none
        let selfUserId = context.coreUserState.selfInfo.userId
        if seatList.contains(where: { $0.userId == selfUserId }) {
            linkStatus = .linking
        }
        update(linkStatus: linkStatus)
    }
    
    func onStartVoiceRoom() {
        fetchUserList()
    }
    
    func onJoinVoiceRoom(ownerId: String) {
        fetchUserList()
        guard let context = context else { return }
        if context.coreUserState.selfInfo.userId != ownerId {
            checkFollowType(ownerId)
        }
    }
    
    func onStopVoiceRoom() {
        update(linkStatus: .none)
    }
    
    func onLeaveVoiceRoom() {
        update(linkStatus: .none)
    }
    
    func followUser(_ user: TUIUserInfo, isFollow: Bool) {
        Task {
            guard let service = service else { return }
            do {
                if isFollow {
                    try await service.followUser(userId: user.userId)
                } else {
                    try await service.unfollowUser(userId: user.userId)
                }
                updateFollowUserList(user: user, isFollow: isFollow)
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
}

// MARK: - Tools
extension VRUserManager {
    private func fetchUserList() {
        Task {
            guard let service = service else { return }
            do {
                let userList = try await service.fetchUserList()
                update { state in
                    state.userList = userList
                }
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    private func checkFollowType(_ userId: String) {
        Task {
            guard let service = service else { return }
            do {
                let type = try await service.checkFollowType(userId: userId)
                var user: TUIUserInfo = TUIUserInfo()
                user.userId = userId
                let isFollow = type == .FOLLOW_TYPE_IN_MY_FOLLOWING_LIST || type == .FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST
                updateFollowUserList(user: user, isFollow: isFollow)
            } catch let err as InternalError {
                toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    private func update(linkStatus: LinkStatus) {
        update { state in
            state.linkStatus = linkStatus
        }
    }
}

// MARK: - VRRoomEngineObserverUserInterface
extension VRUserManager {
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        update(userState: { state in
            if !state.userList.contains(userInfo) {
                state.userList.append(userInfo)
            }
        })
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        update(userState: { state in
            state.userList.removeAll { user in
                user.userId == userInfo.userId
            }
        })
    }
}

// MARK: - VRIMObserverInterface
extension VRUserManager {
    func onMyFollowingListChanged(userInfoList: [V2TIMUserFullInfo], isAdd: Bool) {
        guard var myFollowingUserList = context?.userManager.state.myFollowingUserList else { return }
        if isAdd {
            let newFollowingUsers = userInfoList.map { TUIUserInfo(userFullInfo: $0) }
            myFollowingUserList.formUnion(newFollowingUsers)
        } else {
            let userIdsToRemove = Set(userInfoList.map { $0.userID })
            myFollowingUserList = myFollowingUserList.filter { !userIdsToRemove.contains($0.userId) }
        }
        update(userState: { state in
            state.myFollowingUserList = myFollowingUserList
        })
    }
}

// MARK: - Private functions
extension VRUserManager {
    private typealias UserStateUpdateClosure = (inout VRUserState) -> Void
    
    private func update(userState: UserStateUpdateClosure) {
        observerState.update(reduce: userState)
    }
    
    private func updateFollowUserList(user: TUIUserInfo, isFollow: Bool) {
        update { state in
            if isFollow {
                if !state.myFollowingUserList.map({ $0.userId }).contains(user.userId) {
                    state.myFollowingUserList.insert(user)
                }
            } else {
                let followUserList = state.myFollowingUserList.filter({ $0.userId == user.userId })
                followUserList.forEach { user in
                    state.myFollowingUserList.remove(user)
                }
            }
        }
    }
}
