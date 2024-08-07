//
//  SeatActions.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import RTCRoomEngine


enum SeatActions {
    static let key = "Seat.action"
    // seat list
    static let fetchSeatList = ActionTemplate(id: key.appending(".fetchSeatList"))
    // seat operation
    static let takeSeat = ActionTemplate(id: key.appending(".take"), payloadType: Int?.self)
    static let lockSeat = ActionTemplate(id: key.appending(".lock"), payloadType: (Int, TUISeatLockParams).self)
    static let switchSeat = ActionTemplate(id: key.appending(".switch"), payloadType: Int.self)
    static let leaveSeat = ActionTemplate(id: key.appending(".leave"))
    static let kickSeat = ActionTemplate(id: key.appending(".kick"), payloadType: SeatInfo.self)

    // application take seat operate action.
    static let updateMySeatApplicationId = ActionTemplate(id: key.appending(".updateMySeatApplicationId"), payloadType: String.self)
    static let fetchSeatApplicationList = ActionTemplate(id: key.appending(".fetchApplicationList"))
    static let onFetchSeatApplicationList = ActionTemplate(id: key.appending(".onFetchSeatApplicationList"), payloadType: [SeatApplication].self)
    static let responseSeatApplication = ActionTemplate(id: key.appending(".responseSeatApplication"), payloadType: (Bool, String).self)
    static let cancelApplication = ActionTemplate(id: key.appending(".cancelApplication"), payloadType: String.self)
    static let handleApplicationSuccess =  ActionTemplate(id: key.appending(".handleApplicationSuccess"))
    static let addSeatApplication = ActionTemplate(id: key.appending(".addSeatApplication"), payloadType:SeatApplication.self)
    static let removeSeatApplication = ActionTemplate(id: key.appending(".removeSeatApplication"), payloadType:String.self)
    static let addSeatApplicationUser = ActionTemplate(id: key.appending(".addSeatApplicationUser"), payloadType: User.self)
    
    // invite take seat operate action
    static let inviteSeat = ActionTemplate(id: key.appending(".inviteSeat"), payloadType: (Int, User).self)
    static let cancelInviteSeat = ActionTemplate(id: key.appending(".cancelInviteSeat"), payloadType: SeatInvitation.self)
    static let addSeatInvitation = ActionTemplate(id: key.appending(".addSeatInvitation"), payloadType:SeatInvitation.self)
    static let removeSeatInvitation = ActionTemplate(id: key.appending(".removeSeatInvitation"), payloadType:SeatInvitation.self)
    static let updateReceivedSeatInvitation = ActionTemplate(id: key.appending(".updateMySeatInvitationId"), payloadType: SeatInvitation.self)
    static let responseSeatInvitation = ActionTemplate(id: key.appending(".responseInviteSeat"), payloadType: (Bool, String).self)
    static let onResponseSeatInvitation =  ActionTemplate(id: key.appending(".onResponseInviteSeat"))
    
    // async action
    static let seatListChanged = ActionTemplate(id: key.appending(".seatListChanged"), payloadType: [SeatInfo].self)
}

// MARK: - Subject action, only event, no reduce.
enum SeatResponseActions {
    static let key = "Seat.response"
    static let takeSeatResponse = ActionTemplate(id: key.appending(".takeSeatResponse"), payloadType: TakeSeatResult.self)
}
