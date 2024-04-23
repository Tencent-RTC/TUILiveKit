//
//  AudioEffectReducer.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/3.
//

let audioEffectReducer = Reducer<AudioEffectState>(
    ReduceOn(AudioEffectActions.operateEarMonitor, reduce: { state, action in
        state.isEarMonitorOpened = action.payload
    }),
    ReduceOn(AudioEffectActions.updateEarMonitorVolume, reduce: { state, action in
        state.earMonitorVolume = action.payload
    }),
    ReduceOn(AudioEffectActions.updateMusicVolume, reduce: { state, action in
        state.musicVolume = action.payload
    }),
    ReduceOn(AudioEffectActions.updateMicrophoneVolume, reduce: { state, action in
        state.microphoneVolume = action.payload
    }),
    ReduceOn(AudioEffectActions.updateMusicPitchLevel, reduce: { state, action in
        state.voicePitch = action.payload
    }),
    ReduceOn(AudioEffectActions.reverbVoice, reduce: { state, action in
        state.reverbType = action.payload
    }),
    ReduceOn(AudioEffectActions.changerVoice, reduce: { state, action in
        state.changerType = action.payload
    }),
    ReduceOn(AudioEffectActions.selectMusic, reduce: { state, action in
        state.currentPlayMusic = action.payload
    })
)
