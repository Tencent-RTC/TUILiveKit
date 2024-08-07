//
//  OperationReducer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/6/21.
//

import RTCCommon

let operationReducer = Reducer<OperationState>(
    ReduceOn(OperationActions.clearAllState, reduce: { state, action in
        state.roomState.roomId = ""
        state.roomState.createTime = 0
        state.roomState.roomName = ""
        state.roomState.userCount = 0
        state.roomState.liveExtraInfo.category = .chat
        state.roomState.liveExtraInfo.liveMode = .public
        state.roomState.liveExtraInfo.maxAudienceCount = 0
        state.roomState.liveExtraInfo.messageCount = 0
        state.roomState.liveExtraInfo.giftIncome = 0
        state.roomState.liveExtraInfo.giftPeopleSet = []
        state.roomState.liveExtraInfo.likeCount = 0
        
        state.seatState.mySeatApplicationId = ""
        state.seatState.seatApplicationList = []
        state.seatState.receivedSeatInvitation = SeatInvitation()
        state.seatState.sentSeatInvitationMap = [:]
        state.seatState.seatList = []
        
        state.userState.hasAudioStreamUserList = []
        state.userState.hasVideoStreamUserList = []
        state.userState.myFollowingUserList = []
        state.userState.speakingUserList = []
        state.userState.userList = []
        
        state.mediaState.isMicrophoneOpened = false
        state.mediaState.isMicrophoneMuted = false
        state.mediaState.isCameraOpened = false
    })
)

