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
        state.selfInfo = User(loginInfo: info)
    }),
    ReduceOn(UserActions.onUserAudioAvailable, reduce: { state, action in
        if action.payload.1 {
            state.hasAudioStreamUserList.insert(action.payload.0)
        } else {
            state.hasAudioStreamUserList.remove(action.payload.0)
        }
    }),
    ReduceOn(UserActions.onUserVideoAvailable, reduce: { state, action in
        if action.payload.1 {
            state.hasVideoStreamUserList.insert(action.payload.0)
        } else {
            state.hasVideoStreamUserList.remove(action.payload.0)
        }
    }),
    ReduceOn(UserActions.onUserVoiceVolumeChanged, reduce: {state, action in
        state.speakingUserList = action.payload
    }),
    ReduceOn(UserActions.onUserEnterRoom, reduce: {state, action in
        if !state.userList.contains(action.payload) {
            state.userList.append(action.payload)
        }
    }),
    ReduceOn(UserActions.onUserLeaveRoom, reduce: {state, action in
        state.userList.removeAll { user in
            user.userId == action.payload.userId
        }
    }),
    ReduceOn(UserActions.updateUserList, reduce: {state, action in
        state.userList = action.payload
    }),
    ReduceOn(UserActions.onUserInMyFollowingList, reduce: { state, action in
        if action.payload.1 {
            if !state.myFollowingUserList.map({ $0.userId }).contains(action.payload.0.userId) {
                state.myFollowingUserList.insert(action.payload.0)
            }
        } else {
            let followUserList = state.myFollowingUserList.filter({ $0.userId == action.payload.0.userId })
            followUserList.forEach { user in
                state.myFollowingUserList.remove(user)
            }
        }
    })
)
