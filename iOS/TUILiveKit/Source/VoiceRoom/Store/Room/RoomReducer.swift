//
//  RoomReducer.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//
import RTCRoomEngine

let roomReducer = Reducer<RoomState>(
    ReduceOn(RoomActions.join, reduce: { state, action in
        state.roomId = action.payload.param
    }),
    ReduceOn(RoomActions.joinSuccess, reduce: { state, action in
        let roomInfo = action.payload
        state.roomId = roomInfo.roomId
        state.name = roomInfo.name
        state.ownerId = roomInfo.ownerId
        state.seatMode = roomInfo.seatMode
        state.createTime = roomInfo.createTime
        state.enterRoomState = .inRoom
    }),
    ReduceOn(RoomActions.leaveSuccess, reduce: { state, action in
        state = RoomState()
    }),
    ReduceOn(RoomActions.initializeRoomState, reduce: { state, action in
        state = action.payload
    }),
    ReduceOn(RoomActions.updateRoomName, reduce: { state, action in
        state.name = action.payload
    }),
    ReduceOn(RoomActions.updateRoomCoverUrl, reduce: { state, action in
        state.coverURL = action.payload
    }),
    ReduceOn(RoomActions.updateRoomCategory, reduce: { state, action in
        state.category = action.payload
    }),
    ReduceOn(RoomActions.updateRoomMode, reduce: { state, action in
        state.mode = action.payload
    }),
    ReduceOn(RoomActions.updateRoomMemberCount, reduce: { state, action in
        state.memberCount = action.payload
    })
)

