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
    static let getSeatApplications = Selector.with(getSeatState, keyPath: \SeatState.seatApplications)
    
    static let getMySeatApplicationId = Selector.with(getSeatState, keyPath: \SeatState.mySeatApplicationId)
    
    static func getSeatInfo(index: Int) -> Selector<OperationState, SeatInfo> {
        return Selector.with(getSeatState) { state in
            assert(state.seatList.count > index, "seat index exception，please check seat list in SeatState.")
            return state.seatList[index]
        }
    }
    
    static let getSeatApplicationCount = Selector.with(getSeatState) { state in
        return state.seatApplications.count
    }
    
    static func getSeatApplication(index: Int) -> Selector<OperationState, SeatApplication> {
        return Selector.with(getSeatState) { state in
            assert(state.seatApplications.count > index, "seat application index exception，please check seat list in SeatState.")
            return state.seatApplications[index]
        }
    }
    
    static let getApplicationUserMap = Selector.with(getSeatState) { state in
        return state.seatApplicationUserMap
    }
}
