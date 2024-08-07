//
//  SeatReducer.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import Foundation


let seatReducer = Reducer<SeatState>(
    ReduceOn(SeatActions.seatListChanged, reduce: { state, action in
        state.seatList = action.payload
    }),
    ReduceOn(SeatActions.addSeatApplication, reduce: { state, action in
        let seatApplication = action.payload
        state.seatApplicationList.removeAll(where:{ $0.id == seatApplication.id })
        state.seatApplicationList.append(seatApplication)
    }),
    ReduceOn(SeatActions.removeSeatApplication, reduce: { state, action in
        state.seatApplicationList.removeAll(where:{ $0.id == action.payload })
    }),
    ReduceOn(SeatActions.onFetchSeatApplicationList, reduce: { state, action in
        state.seatApplicationList = action.payload
    }),
    ReduceOn(SeatActions.updateMySeatApplicationId, reduce: { state, action in
        state.mySeatApplicationId = action.payload
    }),
    ReduceOn(SeatActions.updateReceivedSeatInvitation, reduce: { state, action in
        state.receivedSeatInvitation = action.payload
    }),
    ReduceOn(SeatActions.addSeatInvitation, reduce: { state, action in
        state.sentSeatInvitationMap.updateValue(action.payload, forKey: action.payload.userId)
    }),
    ReduceOn(SeatActions.removeSeatInvitation, reduce: { state, action in
        state.sentSeatInvitationMap.removeValue(forKey: action.payload.userId)
    })
)
