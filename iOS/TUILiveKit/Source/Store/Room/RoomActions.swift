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

    static let leave = ActionTemplate(id: key.appending(".leave"))
    static let leaveSuccess = ActionTemplate(id: key.appending(".leaveSuccess"))
    static let stop = ActionTemplate(id: key.appending(".stop"))
    static let stopSuccess = ActionTemplate(id: key.appending(".stopSuccess"))
    
    static let initializeRoomState = ActionTemplate(id: key.appending(".initializeRoomState"), payloadType: RoomState.self)
    static let updateRoomId = ActionTemplate(id: key.appending(".updateRoomId"), payloadType: String.self)
    static let updateRoomName = ActionTemplate(id: key.appending(".updateRoomName"), payloadType: String.self)
    static let updateRoomMemberCount = ActionTemplate(id: key.appending(".updateRoomMemberCount"), payloadType: Int.self)
    static let updateMaxSeatCount = ActionTemplate(id: key.appending(".updateMaxSeatCount"), payloadType: Int.self)
    
    static let updateGiftIncome = ActionTemplate(id: key.appending(".updateGiftIncome"), payloadType: Int.self)
    static let updateGiftPeople = ActionTemplate(id: key.appending(".updateGiftPeople"), payloadType: String.self)
    
    static let fetchRoomOwnerInfo = ActionTemplate(id: key.appending(".fetchRoomOwnerInfo"), payloadType: String.self)
    static let updateRoomOwnerInfo = ActionTemplate(id: key.appending(".updateRoomOwnerInfo"), payloadType: User.self)
    
    static let fetchRoomOwnerFansCount = ActionTemplate(id: key.appending(".fetchRoomOwnerFansCount"), payloadType: String.self)
    static let updateRoomOwnerFansCount = ActionTemplate(id: key.appending(".updateRoomOwnerFansCount"), payloadType: Int.self)
    
    static let fetchRoomInfo = ActionTemplate(id: key.appending(".fetchRoomInfo"))
    static let updateRoomInfo = ActionTemplate(id: key.appending(".updateRoomInfo"),payloadType: TUIRoomInfo.self)
    
    static let fetchLiveInfo = ActionTemplate(id: key.appending(".fetchLiveInfo"), payloadType: String.self)
    
    static let setRoomSeatModeByAdmin = ActionTemplate(id: key.appending(".setRoomSeatModeByAdmin"), payloadType: TUISeatMode.self)
    static let updateRoomSeatModeByAdmin = ActionTemplate(id: key.appending(".updateRoomSeatModeByAdmin"), payloadType: TUISeatMode.self)
    
    static let setRoomCategory = ActionTemplate(id: key.appending(".setRoomCategory"), payloadType: LiveStreamCategory.self)
    static let updateRoomCategory = ActionTemplate(id: key.appending(".updateRoomCategory"), payloadType: LiveStreamCategory.self)
    
    static let setRoomMode = ActionTemplate(id: key.appending(".setRoomMode"), payloadType: LiveStreamPrivacyStatus.self)
    static let updateRoomMode = ActionTemplate(id: key.appending(".updateRoomMode"), payloadType: LiveStreamPrivacyStatus.self)
    
    static let setRoomCoverUrl = ActionTemplate(id: key.appending(".setRoomCoverUrl"), payloadType: String.self)
    static let updateRoomCoverUrl = ActionTemplate(id: key.appending(".updateRoomCoverUrl"), payloadType: String.self)
    
    static let setRoomBackgroundUrl = ActionTemplate(id: key.appending(".setRoomBackgroundUrl"), payloadType: String.self)
    static let updateRoomBackgroundUrl = ActionTemplate(id: key.appending(".updateRoomBackgroundUrl"), payloadType: String.self)
}

// MARK: - Subject action, only event, no reduce.
enum RoomResponseActions {
    static let key = "Room.response"
}
