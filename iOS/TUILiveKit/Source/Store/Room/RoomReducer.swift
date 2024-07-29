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
    ReduceOn(RoomActions.updateRoomOwnerInfo, reduce: { state, action in
        state.ownerInfo = action.payload
    }),
    ReduceOn(RoomActions.leaveSuccess, reduce: { state, action in
        state = RoomState()
    }),
    ReduceOn(RoomActions.initializeRoomState, reduce: { state, action in
        state = action.payload
    }),
    ReduceOn(RoomActions.updateRoomId, reduce: { state, action in
        state.roomId = action.payload
    }),
    ReduceOn(RoomActions.updateRoomName, reduce: { state, action in
        state.roomName = action.payload
    }),
    ReduceOn(RoomActions.updateRoomCoverUrl, reduce: { state, action in
        state.coverURL = action.payload
    }),
    ReduceOn(RoomActions.updateRoomBackgroundUrl, reduce: { state, action in
        state.backgroundURL = action.payload
    }),
    ReduceOn(RoomActions.updateRoomCategory, reduce: { state, action in
        state.liveExtraInfo.category = action.payload
    }),
    ReduceOn(RoomActions.updateRoomMode, reduce: { state, action in
        state.liveExtraInfo.liveMode = action.payload
    }),
    ReduceOn(RoomActions.updateRoomMemberCount, reduce: { state, action in
        state.userCount = action.payload
    }),
    ReduceOn(RoomActions.updateRoomSeatModeByAdmin, reduce: { state, action in
        state.seatMode = action.payload
    }),
    ReduceOn(RoomActions.updateMaxSeatCount, reduce: { state, action in
        state.maxSeatCount = action.payload
    }),
    ReduceOn(RoomActions.updateGiftIncome, reduce: { state, action in
        state.liveExtraInfo.giftIncome += action.payload
    }),
    ReduceOn(RoomActions.updateGiftPeople, reduce: { state, action in
        state.liveExtraInfo.giftPeopleSet.insert(action.payload)
    }),
    ReduceOn(RoomActions.updateRoomOwnerFansCount, reduce: { state, action in
        state.ownerInfo.fansCount = action.payload
    }),
    ReduceOn(RoomActions.updateRoomInfo, reduce: { state, action in
        let roomInfo = action.payload
        state.roomId = roomInfo.roomId
        state.roomName = roomInfo.name
        state.ownerInfo.userId = roomInfo.ownerId
        state.seatMode = roomInfo.seatMode
        state.createTime = roomInfo.createTime
        state.maxSeatCount = roomInfo.maxSeatCount
    })
)
