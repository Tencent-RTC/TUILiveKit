//
//  ViewSelectors.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/27.
//

let viewReducer = Reducer<ViewState>(
    ReduceOn(ViewActions.updateLinkStatus, reduce: { state, action in
        state.linkStatus = action.payload
    }),
    ReduceOn(ViewActions.updateLiveStatus, reduce: { state, action in
        state.liveStatus = action.payload
    }),
    ReduceOn(ViewActions.updateAutoOpenCameraOnSeated, reduce: { state, action in
        state.autoOpenCameraOnSeated = action.payload
    })
)
