//
//  GlobalDefine.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/5.
//


let mediaReducer = Reducer<MediaState>(
    
    ReduceOn(MediaActions.updateMirror, reduce: { state, action in
        state.isMirror = action.payload
    }),
    ReduceOn(MediaActions.operateCamera, reduce: { state, action in
        state.hasCameraPermission = action.payload
    }),
    ReduceOn(MediaActions.operateMicrophone, reduce: { state, action in
        state.hasMicrophonePermission = action.payload
    }),
    ReduceOn(MediaActions.microphoneOpened, reduce: { state, action in
        state.isMicrophoneOpened = true
    }),
    ReduceOn(MediaActions.microphoneClosed, reduce: { state, action in
        state.isMicrophoneOpened = false
    }),
    ReduceOn(MediaActions.microphoneMuted, reduce: { state, action in
        state.isMicrophoneMuted = true
    }),
    ReduceOn(MediaActions.microphoneUnmuted, reduce: { state, action in
        state.isMicrophoneMuted = false
    }),
    ReduceOn(MediaActions.cameraOpened, reduce: { state, action in
        state.isCameraOpened = true
        state.isFrontCamera = action.payload == .front
    }),
    ReduceOn(MediaActions.frontCameraOpened, reduce: { state, action in
        state.isFrontCamera = true
    }),
    ReduceOn(MediaActions.rearCameraOpened, reduce: { state, action in
        state.isFrontCamera = false
    }),
    ReduceOn(MediaActions.cameraClosed, reduce: { state, action in
        state.isCameraOpened = false
    }),
    ReduceOn(MediaActions.videoQualityUpdated, reduce: { state, action in
        state.videoQuality = action.payload
    }),
    ReduceOn(MediaActions.audioQualityUpdated, reduce: { state, action in
        state.audioQuality = action.payload
    })
)
