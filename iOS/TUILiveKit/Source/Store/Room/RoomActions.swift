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
    static let updateRoomId = ActionTemplate(id: key.appending(".updateRoomId"), payloadType: String.self)
    static let updateRoomName = ActionTemplate(id: key.appending(".updateRoomName"), payloadType: String.self)
    static let updateRoomCoverUrl = ActionTemplate(id: key.appending(".updateRoomCoverUrl"), payloadType: String.self)
    static let updateRoomCategory = ActionTemplate(id: key.appending(".updateRoomCategory"), payloadType: LiveStreamCategory.self)
    static let updateRoomMode = ActionTemplate(id: key.appending(".updateRoomMode"), payloadType: LiveStreamPrivacyStatus.self)
    static let updateRoomMemberCount = ActionTemplate(id: key.appending(".updateRoomMemberCount"), payloadType: Int.self)
    
    static let updateGiftIncome = ActionTemplate(id: key.appending(".updateGiftIncome"), payloadType: Int.self)
    static let updateGiftPeople = ActionTemplate(id: key.appending(".updateGiftPeople"), payloadType: String.self)
    
    static let fetchRoomOwnerInfo = ActionTemplate(id: key.appending(".fetchRoomOwnerInfo"))
    static let updateRoomOwnerInfo = ActionTemplate(id: key.appending(".updateRoomOwnerInfo"), payloadType: User.self)
    
    static let fetchRoomOwnerFansCount = ActionTemplate(id: key.appending(".fetchRoomOwnerFansCount"), payloadType: String.self)
    static let updateRoomOwnerFansCount = ActionTemplate(id: key.appending(".updateRoomOwnerFansCount"), payloadType: Int.self)
    static let fetchRoomInfo = ActionTemplate(id: key.appending(".fetchRoomInfo"))
    static let updateRoomInfo = ActionTemplate(id: key.appending(".updateRoomInfo"),payloadType: TUIRoomInfo.self)
    static let updateLiveInfo = ActionTemplate(id: key.appending(".updateLiveInfo"),payloadType: (TUILiveInfo, TUILiveModifyFlag).self)
    static let onUpdateLiveInfoSuccess = ActionTemplate(id: key.appending(".onUpdateLiveInfoSuccess"))
}

// MARK: - Subject action, only event, no reduce.
enum RoomResponseActions {
    static let key = "Room.response"
}
