//
//  RoomSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

enum RoomSelectors {
    static let getRoomState = Selector(keyPath: \OperationState.roomState)
    
    static let getRoomId = Selector.with(getRoomState, keyPath: \RoomState.roomId)
    static let getRoomName = Selector.with(getRoomState, keyPath: \RoomState.roomName)
    static let getUserCount = Selector.with(getRoomState, keyPath:\RoomState.userCount)
    static let getRoomCoverUrl = Selector.with(getRoomState) { value in
        let url = URL(string: value.coverURL)
        return url
    }
    
    static let roomOwnerId = Selector(keyPath: \OperationState.roomState.ownerInfo.userId)
    static let roomOwnerAvatar = Selector(keyPath: \OperationState.roomState.ownerInfo.avatarUrl)
    static let roomOwnerName = Selector(keyPath: \OperationState.roomState.ownerInfo.name)
    static let getRoomOwnerInfo = Selector(keyPath: \OperationState.roomState.ownerInfo)
    static let getRoomOwnerFansCount = Selector(keyPath: \OperationState.roomState.ownerInfo.fansCount)
    
    // custom state
    static let getCategory = Selector.with(getRoomState, keyPath:\RoomState.liveExtraInfo.category)
    static let getLiveMode = Selector.with(getRoomState, keyPath:\RoomState.liveExtraInfo.liveMode)
    
    static let getGiftIncome = Selector.with(getRoomState, keyPath: \RoomState.liveExtraInfo.giftIncome)
    static let getGiftPeopleSet = Selector.with(getRoomState, keyPath: \RoomState.liveExtraInfo.giftPeopleSet)
}
