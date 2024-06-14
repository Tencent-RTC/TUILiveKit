//
//  RoomListReducer.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Combine

let roomListNavigationReducer = Reducer<RoomListNavigationState>(
    ReduceOn(RoomListNavigatorActions.navigatorTo, reduce: { state, action in
        state.currentRouter = action.payload
    })
)

let roomListReducer = Reducer<RoomListState>(
    ReduceOn(RoomListActions.updateRoomInfoList, reduce: { state, action in
        state.roomInfoListResult.cursor = action.payload.cursor
        state.roomInfoListResult.roomInfoList = action.payload.roomInfoList
    })
)
