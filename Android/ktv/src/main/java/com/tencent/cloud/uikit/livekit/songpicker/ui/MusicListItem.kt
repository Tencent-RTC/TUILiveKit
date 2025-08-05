package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import coil3.compose.AsyncImage
import com.tencent.cloud.uikit.livekit.songpicker.LocalViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.Song

@Composable
fun MusicListItem(musicInfo: Song, isSelected: Boolean) {
    val viewModel = LocalViewModel.current
    Row(
        horizontalArrangement = Arrangement.Start,
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 15.dp, vertical = 5.dp)
    ) {
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
                musicInfo.singers.toString().replace("[", "").replace("]", ""),
                color = COLOR_GRAY60,
                fontSize = 14.sp
            )
        }
        Spacer(modifier = Modifier.weight(1F))
        if (isSelected) {
            Row(
                horizontalArrangement = Arrangement.Center,
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier
                    .width(80.dp)
                    .height(30.dp)
                    .border(
                        width = 1.dp,
                        color = COLOR_GRAY60,
                        shape = RoundedCornerShape(15.dp)
                    )
                    .clickable {}
            ) {
                Text(
                    stringResource(R_STRING_ID_CHOSEN_SONG),
                    color = COLOR_GRAY60,
                    fontSize = 14.sp
                )
            }
        } else {
            Row(
                horizontalArrangement = Arrangement.Center,
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier
                    .width(80.dp)
                    .height(30.dp)
                    .clip(RoundedCornerShape(15.dp))
                    .background(COLOR_BRUSH_SELECTED)
                    .clickable {
                        viewModel.songPickerStore.addMusicToPlaylist(musicInfo)
                    }
            ) {
                Text(
                    stringResource(R_STRING_ID_CHOOSE_SONG),
                    color = Color.White,
                    fontSize = 14.sp
                )
            }
        }
    }
}