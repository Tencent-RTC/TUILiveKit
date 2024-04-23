//
//  RoomActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import RTCRoomEngine

enum RoomActions {
    static let key = "Room.action"
    static let start = ActionTemplate(id: key.appending(".start"), payloadType: NextActionParamTuple<TUIRoomInfo>.self)
    static let join = ActionTemplate(id: key.appending(".join"), payloadType: NextActionParamTuple<String>.self)
    static let joinSuccess = ActionTemplate(id: key.appending(".joinSuccess"), payloadType: TUIRoomInfo.self)
    static let leave = ActionTemplate(id: key.appending(".leave"))
    static let leaveSuccess = ActionTemplate(id: key.appending(".leaveSuccess"))
    static let stop = ActionTemplate(id: key.appending(".stop"))
    static let stopSuccess = ActionTemplate(id: key.appending(".stopSuccess"))
    
    static let initializeRoomState = ActionTemplate(id: key.appending(".initializeRoomState"), payloadType: RoomState.self)
    static let updateRoomName = ActionTemplate(id: key.appending(".updateRoomName"), payloadType: String.self)
    static let updateRoomCoverUrl = ActionTemplate(id: key.appending(".updateRoomCoverUrl"), payloadType: String.self)
    static let updateRoomCategory = ActionTemplate(id: key.appending(".updateRoomCategory"), payloadType: String.self)
    static let updateRoomMode = ActionTemplate(id: key.appending(".updateRoomMode"), payloadType: String.self)
    static let updateRoomMemberCount = ActionTemplate(id: key.appending(".updateRoomMemberCount"), payloadType: Int.self)
}
