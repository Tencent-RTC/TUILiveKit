package com.tencent.cloud.uikit.livekit.karaoke.state

import kotlinx.coroutines.flow.StateFlow

enum class PlayState(val rawValue: String) {
    STOPPED("stopped"),
    PLAYING("playing"),
}

enum class LiveStatus {
    IDLE,
    LIVE,
    ENDED,
}

data class MusicPitchModel(
    val startTime: Long,
    val duration: Long,
    val pitch: Int
)

data class KaraokeState(
    val isAccompany: StateFlow<Boolean>,
    val currentPlayProgress: StateFlow<Long>,
    val currentPlayDuration: StateFlow<Long>,
    val playState: StateFlow<PlayState>,
    val standardPitchModels: StateFlow<List<MusicPitchModel>>,
    val currentPitch: StateFlow<Int>,
    val playScore: StateFlow<Int>,
    val isMicrophoneMuted: StateFlow<Boolean>,
    val liveStatus: StateFlow<LiveStatus>,
)