package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.wrapContentWidth
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.ScrollableTabRow
import androidx.compose.material3.Tab
import androidx.compose.material3.TabRow
import androidx.compose.material3.TabRowDefaults
import androidx.compose.material3.TabRowDefaults.tabIndicatorOffset
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import com.tencent.cloud.uikit.livekit.songpicker.LocalViewModel

@Composable
fun MusicLibraryMainLayout() {
    val viewModel = LocalViewModel.current
    var selectedIndex by remember { mutableIntStateOf(viewModel.songPickerStore.currentRequestIndex.value) }
    val musicSelectedList by viewModel.songPickerStore._musicSelectedList.collectAsStateWithLifecycle()
    val tabList = listOf(
        stringResource(R_STRING_ID_CHOOSE_SONG),
        if (musicSelectedList.isEmpty()) stringResource(R_STRING_ID_CHOSEN_SONG) else stringResource(
            id = R_STRING_ID_CHOSEN_SONG_COUNT,
            musicSelectedList.size
        ),
    )
    viewModel.songPickerStore.updateMusicTagList()
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .height(400.dp)
            .clip(RoundedCornerShape(topStart = 15.dp, topEnd = 15.dp))
            .background(COLOR_BRUSH_DIALOG_CONTENT)
    ) {
        TabRow(
            selectedTabIndex = selectedIndex,
            containerColor = Color.Transparent,
            divider = {
                HorizontalDivider(color = Color.Gray)
            },
            indicator = { tabPositions ->
                TabRowDefaults.PrimaryIndicator(
                    modifier = Modifier.tabIndicatorOffset(tabPositions[selectedIndex]),
                    color = COLOR_SELECTED
                )
            }
        ) {
            tabList.forEachIndexed { index, text ->
                val selected = selectedIndex == index
                val color = if (selected) COLOR_SELECTED else COLOR_NORMAL
                Tab(
                    selected = selected,
                    onClick = {
                        selectedIndex = index
                        viewModel.songPickerStore.setCurrentRequestIndex(index)
                    },
                    text = { Text(text = text, color = color, fontSize = 18.sp) }
                )
            }
        }
        if (selectedIndex == 0) {
            MusicListLayout()
        } else {
            MusicSelectedListLayout()
        }
    }
}

@Composable
fun MusicListLayout() {
//    MusicSearchEdit()
//    MusicTagListLayout()
    MusicTagListLayout2()
}

@Composable
fun MusicSelectedListLayout() {
    MusicSelectedList()
}

@Composable
fun MusicSearchEdit() {
    var showDialog by remember { mutableStateOf(false) }
    if (showDialog) {
        Dialog(
            properties = DialogProperties(
                usePlatformDefaultWidth = false,
                dismissOnClickOutside = true
            ),
            onDismissRequest = { showDialog = false }
        ) {
            MusicSearchLayout(onCancel = { showDialog = false })
        }
    }
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .height(60.dp)
            .padding(10.dp)
            .clip(RoundedCornerShape(20.dp))
            .background(COLOR_GRAY33)
            .clickable {
                showDialog = true
            },
        horizontalArrangement = Arrangement.Start,
        verticalAlignment = Alignment.CenterVertically
    ) {
        Spacer(modifier = Modifier.width(10.dp))
        Icon(
            Icons.Filled.Search,
            contentDescription = "",
            modifier = Modifier.size(20.dp),
            tint = Color.White
        )
        Spacer(modifier = Modifier.width(5.dp))
        Text(
            stringResource(R_STRING_ID_SEARCH_MUSIC_HINT),
            color = COLOR_GRAY60,
            fontSize = 14.sp
        )
    }
}

@Composable
fun MusicTagListLayout() {
    val viewModel = LocalViewModel.current
    var selectedIndex by remember { mutableIntStateOf(0) }
    val musicTags by viewModel.songPickerStore.musicTagList.collectAsStateWithLifecycle()
    val tabList = remember(musicTags) {
        musicTags.map { it.name }
    }
    if (musicTags.isEmpty()) {
        return
    }
    ScrollableTabRow(
        modifier = Modifier.wrapContentWidth(),
        selectedTabIndex = selectedIndex,
        containerColor = Color.Transparent,
        divider = {},
        indicator = {}
    ) {
        tabList.forEachIndexed { index, text ->
            val selected = selectedIndex == index
            val textColor = COLOR_NORMAL
            val backColor = if (selected) COLOR_MUSIC_TAG_SELECTED else COLOR_MUSIC_TAG_NORMAL
            Tab(selected = selected, onClick = { selectedIndex = index }) {
                Box(
                    modifier = Modifier
                        .padding(10.dp)
                        .clip(RoundedCornerShape(15.dp))
                        .background(backColor)
                ) {
                    Text(
                        text = text,
                        color = textColor,
                        fontSize = 14.sp,
                        modifier = Modifier.padding(horizontal = 10.dp, vertical = 5.dp)
                    )
                }
            }
        }
    }
    MusicList(musicTags.get(selectedIndex).id)
}


@Composable
fun MusicTagListLayout2() {
    val viewModel = LocalViewModel.current
    val musicTags by viewModel.songPickerStore.musicTagList.collectAsStateWithLifecycle()
    if (musicTags.isEmpty()) {
        return
    }
    MusicList(musicTags.get(0).id)
}