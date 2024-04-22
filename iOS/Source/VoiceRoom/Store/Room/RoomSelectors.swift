//
//  RoomSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

enum RoomSelectors {
    static let getRoomState = Selector(keyPath: \OperationState.roomState)
    static let getRoomId = Selector.with(getRoomState, keyPath: \RoomState.roomId)
    static let getRoomName = Selector.with(getRoomState, keyPath: \RoomState.name)
    static let getMemberCount = Selector.with(getRoomState, keyPath:\RoomState.memberCount)
    static let getRoomCoverUrl = Selector.with(getRoomState) { value in
        let url = URL(string: value.coverURL)
        return url
    }
    
    static let getEnterRoomState = Selector.with(getRoomState, keyPath: \RoomState.enterRoomState)
    static let getRoomOwnerId = Selector(keyPath: \OperationState.roomState.ownerId)
    
    // custom state
    static let getCategory = Selector.with(getRoomState, keyPath:\RoomState.category)
    static let getMode = Selector.with(getRoomState, keyPath:\RoomState.mode)
}
