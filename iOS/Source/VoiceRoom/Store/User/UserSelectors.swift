//
//  UserSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

enum UserSelectors {
    static let getSelfInfo = Selector(keyPath: \OperationState.userState.currentUser)
    static let getUserState = Selector(keyPath: \OperationState.userState)
    static let getRoomOwnerInfo = Selector(keyPath: \OperationState.userState.roomOwner)
    
    static let getMemberAvatars = Selector.with(getUserState) { state in
        let avatars: [String] = state.audienceList.prefix(2).map { user in
            return user.avatarUrl
        }
        return avatars
    }
    static let currentUserId = Selector(keyPath: \OperationState.userState.currentUser.userId)
    static let isOnSeat = Selector.with(SeatSelectors.getSeatList, getSelfInfo) { seatList, user in
        return seatList.contains { $0.userId == user.userId }
    }
    static let isOwner = Selector.with(UserSelectors.getSelfInfo, RoomSelectors.getRoomOwnerId) { user, ownerId in
        return user.userId == ownerId
    }
    
    static let getAudioAvailableUsers = Selector.with(getUserState, keyPath: \UserState.audioAvailableUsers)
    static let getSpeakingUsers = Selector.with(getUserState, keyPath: \UserState.speakingUsers)
    static let getAudienceList = Selector.with(getUserState, keyPath: \UserState.audienceList)
    
    static let getReceivedGiftTotalPrice = Selector.with(getUserState, keyPath: \UserState.receivedGiftTotalPrice)
    static let getSendGiftUsers = Selector.with(getUserState, keyPath: \UserState.sendGiftUsers)
}
