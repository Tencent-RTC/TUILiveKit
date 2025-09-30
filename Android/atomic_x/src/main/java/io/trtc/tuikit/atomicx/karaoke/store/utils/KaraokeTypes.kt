package io.trtc.tuikit.atomicx.karaoke.store.utils

import io.trtc.tuikit.atomicx.R

enum class PlaybackState {
    IDLE, START, PAUSE, RESUME, STOP
}

data class MusicInfo(
    var musicId: String = "",
    var musicName: String = "",
    var artist: List<String> = emptyList(),
    var lyricUrl: String = "",
    var duration: Int = 0,
    var originalUrl: String = "",
    var accompanyUrl: String = "",
    var coverUrl: Int = R.drawable.karaoke_song_cover,
)

data class MusicSelection(
    var musicId: String = "",
    var userId: String = "",
    var userName: String = "",
    var avatarUrl: String = "",
)

enum class LyricAlign {
    RIGHT,
    CENTER
}
