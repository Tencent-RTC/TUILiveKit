//
//  UserReducer.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import RTCRoomEngine

let userReducer = Reducer<UserState>(
    ReduceOn(UserActions.getSelfInfo, reduce: { state, action in
        let info = TUIRoomEngine.getSelfInfo()
        state.currentUser = User(loginInfo: info)
    }),
    ReduceOn(UserActions.onUserAudioAvailable, reduce: { state, action in
        if action.payload.1 {
            state.audioAvailableUsers.insert(action.payload.0)
        } else {
            state.audioAvailableUsers.remove(action.payload.0)
        }
    }),
    ReduceOn(UserActions.onUserVoiceVolumeChanged, reduce: {state, action in
        state.speakingUsers = action.payload
    }),
    ReduceOn(UserActions.onUserEnterRoom, reduce: {state, action in
        state.audienceList.append(action.payload)
    }),
    ReduceOn(UserActions.onUserLeaveRoom, reduce: {state, action in
        state.audienceList.removeAll { user in
            user.userId == action.payload.userId
        }
    }),
    ReduceOn(UserActions.updateUserList, reduce: {state, action in
        state.audienceList = action.payload
    }),
    ReduceOn(UserActions.updateReceivedGiftTotalPrice, reduce: { state, action in
        state.receivedGiftTotalPrice += action.payload
    }),
    ReduceOn(UserActions.updateSendGiftUser, reduce: { state, action in
        state.sendGiftUsers.insert(action.payload)
    }),
    ReduceOn(UserActions.updateRoomOwnerInfo, reduce: { state, action in
        state.roomOwner = action.payload
    })
)
