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
        state.seatApplications.removeAll(where:{ $0.id == seatApplication.id })
        state.seatApplications.append(seatApplication)
    }),
    ReduceOn(SeatActions.removeSeatApplication, reduce: { state, action in
        if let application = state.seatApplications.first(where: { $0.id == action.payload }) {
            state.seatApplicationUserMap.removeValue(forKey: application.userId)
        }
        state.seatApplications.removeAll(where:{ $0.id == action.payload })
    }),
    ReduceOn(SeatActions.addSeatApplicationUser, reduce: { state, action in
        let user = action.payload
        state.seatApplicationUserMap[user.userId] = user
    }),
    ReduceOn(SeatActions.onSeatApplicationListUpdate, reduce: { state, action in
        state.seatApplications = action.payload
    }),
    ReduceOn(SeatActions.updateMySeatApplicationId, reduce: { state, action in
        state.mySeatApplicationId = action.payload
    })
)
