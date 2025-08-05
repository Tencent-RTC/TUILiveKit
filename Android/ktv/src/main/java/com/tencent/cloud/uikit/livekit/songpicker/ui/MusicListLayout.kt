package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.remember
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import com.tencent.cloud.uikit.livekit.songpicker.LocalViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.Song

@Composable
fun MusicList(musicTagId: String) {
    val viewModel = LocalViewModel.current
    val list = remember { mutableStateListOf<Song>() }
    val hasLoadMusicsByTag by viewModel.songPickerStore.hasLoadMusicsByTag.collectAsStateWithLifecycle()
    val musicSelectedList by viewModel.songPickerStore._musicSelectedList.collectAsStateWithLifecycle()
    viewModel.songPickerStore.loadMusicsByTagId(musicTagId)
    if (musicTagId == hasLoadMusicsByTag) {
        viewModel.songPickerStore.musicInfoCache.get(musicTagId)?.let {
            list.clear()
            list.addAll(it)
        }
    }
    LazyColumn {
        itemsIndexed(list) { index, musicInfo ->
            val isSelected = musicSelectedList.any { musicInfo.id == it.id }
            MusicListItem(musicInfo, isSelected)
        }
    }
}