//
//  RoomListActions.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import RTCRoomEngine

enum RoomListNavigatorActions {
    static let key = "RoomList.navigation.Action"
    static let navigatorTo = ActionTemplate(id: key.appending(".present"), payloadType: RoomListNavigationState.Router.self)
}

enum RoomListActions {
    static let key = "RoomList.action"
    static let getRoomInfoList = ActionTemplate(id: key.appending(".getRoomInfoList"), payloadType: String.self)
    static let updateRoomInfoList = ActionTemplate(id: key.appending(".updateRoomInfoList"), payloadType: RoomListResult.self)
    static let toastError = ActionTemplate(id: key.appending("toastError"), payloadType: InternalError.self)
}
