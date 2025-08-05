package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import coil3.compose.AsyncImage
import com.tencent.cloud.uikit.livekit.songpicker.LocalViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.Song

@Composable
fun MusicSelectedList() {
    val viewModel = LocalViewModel.current
    val musicSelectedList by viewModel.songPickerStore._musicSelectedList.collectAsStateWithLifecycle()
    if (musicSelectedList.isEmpty()) {
        Column(
            verticalArrangement = Arrangement.Center,
            horizontalAlignment = Alignment.CenterHorizontally,
            modifier = Modifier.fillMaxSize()
        ) {
            Text(stringResource(R_STRING_ID_NO_SONG_CHOSEN), color = Color.Gray, fontSize = 16.sp)
            Spacer(modifier = Modifier.height(20.dp))
            Text(
                stringResource(R_STRING_ID_GOTO_ORDER_SONG),
                color = Color.Gray.copy(alpha = 0.7F),
                fontSize = 14.sp
            )
        }
    } else {
        LazyColumn {
            itemsIndexed(musicSelectedList) { index, musicInfo ->
                MusicSelectedListItem(index, musicInfo)
            }
        }
    }
}

@Composable
fun MusicSelectedListItem(index: Int, musicInfo: Song) {
    val viewModel = LocalViewModel.current
    Row(
        horizontalArrangement = Arrangement.Start,
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 15.dp, vertical = 5.dp)
    ) {
        if (index == 0) {
            Image(
                painter = painterResource(R_DRAWABLE_ID_PLAYING_ICON),
                contentDescription = "",
                modifier = Modifier.size(30.dp)
            )
        } else {
            Box(modifier = Modifier.size(30.dp), contentAlignment = Alignment.Center) {
                Text("${index + 1}", textAlign = TextAlign.Center, color = COLOR_NORMAL)
            }
        }
        Spacer(modifier = Modifier.width(15.dp))
        AsyncImage(
            model = musicInfo.coverUrl,
            contentDescription = "",
            modifier = Modifier
                .size(64.dp)
                .clip(RoundedCornerShape(15.dp)),
            error = painterResource(R_DRAWABLE_ID_MUSIC_DEFAULT),
        )
        Column(Modifier.padding(horizontal = 5.dp)) {
            Text(musicInfo.name, color = COLOR_NORMAL, fontSize = 16.sp)
            Spacer(modifier = Modifier.height(5.dp))
            Text(
                stringResource(
                    id = R_STRING_ID_SINGER,
                    musicInfo.singers.toString().replace("[", "").replace("]", "")
                ),
                color = COLOR_GRAY60,
                fontSize = 14.sp
            )
        }
        Spacer(modifier = Modifier.weight(1F))
        Box(modifier = Modifier.clickable(onClick = {
            if (index == 0) {
                viewModel.songPickerStore.deleteMusicFromPlaylist(musicInfo)
            } else {
                viewModel.songPickerStore.topMusic(musicInfo)
            }
        })) {
            val resId =
                if (index == 0) R_DRAWABLE_ID_NEXT_SONG else R_DRAWABLE_ID_SET_TOP_HOVER
            Icon(
                painterResource(resId),
                "",
                tint = if (index == 1) Color.Gray else Color.Unspecified,
                modifier = Modifier.size(44.dp)
            )
        }
        if (index > 0) {
            Spacer(modifier = Modifier.weight(1F))
            Box(
                modifier = Modifier
                    .clickable(onClick = {
                        viewModel.songPickerStore.deleteMusicFromPlaylist(musicInfo)
                    })
            ) {
                Icon(
                    painterResource(R_DRAWABLE_ID_DELETE_SONG),
                    "",
                    tint = Color.Unspecified,
                    modifier = Modifier.size(44.dp)
                )
            }
        }
    }
}