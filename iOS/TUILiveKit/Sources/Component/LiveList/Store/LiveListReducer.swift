//
//  LiveListReducer.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Combine

let liveListNavigationReducer = Reducer<LiveListNavigationState>(
    ReduceOn(LiveListNavigatorActions.navigatorTo, reduce: { state, action in
        state.currentRouter = action.payload
    })
)

let liveListReducer = Reducer<LiveListState>(
    ReduceOn(LiveListActions.updateLiveInfoList, reduce: { state, action in
        state.liveInfoListResult.cursor = action.payload.cursor
        state.liveInfoListResult.liveInfoList = action.payload.liveInfoList
    })
)
