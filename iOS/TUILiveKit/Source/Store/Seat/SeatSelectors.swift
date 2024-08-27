//
//  SeatSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//
import RTCRoomEngine

enum SeatSelectors {
    static let getSeatState = Selector(keyPath: \OperationState.seatState)
    static let getSeatList = Selector.with(getSeatState, keyPath: \.seatList)
    static let getSeatCount = Selector.with(getSeatList) { seatList in
        return seatList.count
    }
    static let getSeatUserCount = Selector.with(getSeatList) { seatList in
        return seatList.filter({!$0.userId.isEmpty}).count
    }
    static let getSeatApplications = Selector.with(getSeatState, keyPath: \SeatState.seatApplicationList)
    
    static let getMySeatApplicationId = Selector.with(getSeatState, keyPath: \SeatState.mySeatApplicationId)
    
    static func getSeatInfo(index: Int) -> Selector<OperationState, SeatInfo> {
        return Selector.with(getSeatState) { state in
            if state.seatList.isEmpty { return SeatInfo() }
            assert(state.seatList.count > index, "seat index exception，please check seat list in SeatState.")
            return state.seatList[index]
        }
    }
    
    static let getSeatApplicationCount = Selector.with(getSeatState) { state in
        return state.seatApplicationList.count
    }
    
    static func getSeatApplication(index: Int) -> Selector<OperationState, SeatApplication> {
        return Selector.with(getSeatState) { state in
            assert(state.seatApplicationList.count > index, "seat application index exception，please check seat list in SeatState.")
            return state.seatApplicationList[index]
        }
    }
    
    static let getSeatInvitationMap = Selector.with(getSeatState, keyPath: \SeatState.sentSeatInvitationMap)
    
    static let getReceivedSeatInvitation = Selector.with(getSeatState, projector: \SeatState.receivedSeatInvitation)
}
