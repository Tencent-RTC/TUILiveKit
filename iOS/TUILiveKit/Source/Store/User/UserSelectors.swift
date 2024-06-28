//
//  UserSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

enum UserSelectors {
    static let getSelfInfo = Selector(keyPath: \OperationState.userState.selfInfo)
    static let getUserState = Selector(keyPath: \OperationState.userState)
    
    static let currentUserId = Selector(keyPath: \OperationState.userState.selfInfo.userId)
    static let isOnSeat = Selector.with(SeatSelectors.getSeatList, getSelfInfo) { seatList, user in
        return seatList.contains { $0.userId == user.userId }
    }
    static let isOwner = Selector.with(UserSelectors.getSelfInfo, RoomSelectors.roomOwnerId) { user, ownerId in
        return user.userId == ownerId
    }
    
    static let getHasAudioStreamUserList = Selector.with(getUserState, keyPath: \UserState.hasAudioStreamUserList)
    static let getHasVideoStreamUserList = Selector.with(getUserState, keyPath: \UserState.hasVideoStreamUserList)
    static let getUserList = Selector.with(getUserState, keyPath: \UserState.userList)
    static let getHasAudioVolumeUserList = Selector.with(getUserState, keyPath: \UserState.speakingUserList)
    static let getMyFollowingList = Selector.with(getUserState, keyPath: \UserState.myFollowingUserList)
}
