//
//  GlobalDefine.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/5.
//

let mediaReducer = Reducer<MediaState>(
    ReduceOn(MediaActions.microphoneOpened, reduce: { state, action in
        state.isMicrophoneOpened = true
    }),
    ReduceOn(MediaActions.microphoneClosed, reduce: { state, action in
        state.isMicrophoneOpened = false
    }),
    ReduceOn(MediaActions.operateVoiceEarMonitor, reduce: { state, action in
        state.isEarMonitorOpened = action.payload
    }),
    ReduceOn(MediaActions.localAudioMuted, reduce: { state, action in
        state.isLocalAudioStreamMuted = true
    }),
    ReduceOn(MediaActions.localAudioUnmuted, reduce: { state, action in
        state.isLocalAudioStreamMuted = false
    })
)
