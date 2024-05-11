//
//  MusicPanelReducer.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

let musicPanelReducer = Reducer<MusicPanelState>(
    ReduceOn(MusicPanelActions.startPlayMusic, reduce: { state, action in
        state.currentPlayMusic = action.payload
    }),
    ReduceOn(MusicPanelActions.onMusicPlaySuccess, reduce: { state, action in
        guard let currentPlayMusic = state.currentPlayMusic else { return }
        if let index = state.musicInfoList.firstIndex(where: { $0.id == currentPlayMusic.id }) {
            state.musicInfoList[index].isPlaying = true
            state.currentPlayMusic?.isPlaying = true
        }
    }),
    ReduceOn(MusicPanelActions.onMusicPlayComplete, reduce: { state, action in
        guard let currentPlayMusic = state.currentPlayMusic else { return }
        if let index = state.musicInfoList.firstIndex(where: { $0.id == currentPlayMusic.id }) {
            state.musicInfoList[index].isPlaying = false
            state.currentPlayMusic?.isPlaying = false
        }
    }),
    ReduceOn(MusicPanelActions.onMusicPlayError, reduce: { state, action in
        guard let currentPlayMusic = state.currentPlayMusic else { return }
        if let index = state.musicInfoList.firstIndex(where: { $0.id == currentPlayMusic.id }) {
            state.musicInfoList[index].isPlaying = false
            state.currentPlayMusic?.isPlaying = false
        }
    }),
    ReduceOn(MusicPanelActions.stopPlayMusic, reduce: { state, action in
        guard let currentPlayMusic = state.currentPlayMusic else { return }
        if let index = state.musicInfoList.firstIndex(where: { $0.id == currentPlayMusic.id }) {
            state.musicInfoList[index].isPlaying = false
            state.currentPlayMusic?.isPlaying = false
            state.currentPlayMusic = nil
        }
    }),
    ReduceOn(MusicPanelActions.deleteMusic, reduce: { state, action in
        state.musicInfoList = state.musicInfoList.filter { musicInfo in
            musicInfo.id != action.payload.id
        }
    })
)
