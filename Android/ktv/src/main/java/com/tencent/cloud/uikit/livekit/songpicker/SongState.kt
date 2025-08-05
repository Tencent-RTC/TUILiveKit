package com.tencent.cloud.uikit.livekit.songpicker

import com.tencent.cloud.uikit.livekit.songpicker.model.Song
import kotlinx.coroutines.flow.StateFlow

data class SongState (
    val songSelectedList: StateFlow<List<Song>>,
    val currentSong: StateFlow<Song>,
)