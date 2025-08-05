package com.tencent.cloud.uikit.livekit.karaoke.view

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
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
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.KeyboardArrowRight
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.PlayState
import com.tencent.cloud.uikit.livekit.karaoke.view.lyric.LyricComposeView
import com.tencent.cloud.uikit.livekit.karaoke.view.patch.MusicPitchView
import com.tencent.cloud.uikit.livekit.KTVViewModel

@Composable
fun MusicPlayerPanel(
    modifier: Modifier = Modifier,
    liveId: String,
    isAnchor: Boolean
) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val currentSong by viewModel.songState.currentSong.collectAsState()
    val isAccompany by viewModel.karaokeState.isAccompany.collectAsState()
    val currentProgress by viewModel.karaokeState.currentPlayProgress.collectAsState()
    val currentDuration by viewModel.karaokeState.currentPlayDuration.collectAsState()
    val playState by viewModel.karaokeState.playState.collectAsState()
    val standardPitchModels by viewModel.karaokeState.standardPitchModels.collectAsState()
    val currentPitch by viewModel.karaokeState.currentPitch.collectAsState()
    if (isAnchor) {
        LaunchedEffect(currentSong) {
            if (currentSong.id.isNotEmpty()) {
                viewModel.karaokeStore.startPlayMusic(
                    currentSong.id,
                    currentSong.originUrl,
                    currentSong.accompanyUrl
                )
            } else {
                viewModel.karaokeStore.stopPlayMusic()
            }
        }

        LaunchedEffect(playState) {
            if (playState == PlayState.STOPPED) {
                viewModel.songPickerStore.deleteMusicFromPlaylist(currentSong)
            }
        }
    }
    val musicPlayText = "%02d:%02d/%02d:%02d".format(
        currentProgress / 60_000,
        currentProgress / 1000 % 60,
        currentDuration / 60_000,
        currentDuration / 1000 % 60
    )

    Box(
        modifier = modifier
            .fillMaxWidth()
            .height(200.dp)
            .clip(RoundedCornerShape(40.dp))
    ) {
        Image(
            painter = painterResource(id = R.drawable.ktv_bg_music_panel),
            contentDescription = null,
            modifier = Modifier.fillMaxSize(),
            contentScale = ContentScale.FillBounds
        )
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .align(Alignment.BottomCenter)
        ) {
            Column {
                if (isAnchor) {
                    if (playState == PlayState.PLAYING) {
                        MusicPitchView(
                            modifier = Modifier
                                .fillMaxWidth()
                                .height(55.dp)
                                .padding(horizontal = 20.dp),
                            standardPitchModels = standardPitchModels,
                            currentProgress = currentProgress,
                            currentPitch = currentPitch,
                        )
                        Spacer(modifier = Modifier.height(10.dp))
                    }
                }
                if (currentSong.id.isNotEmpty()) {
                    LyricComposeView(
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(80.dp),
                        liveId = liveId
                    )
                }
            }
        }
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 20.dp)
                .align(Alignment.TopStart),
            horizontalArrangement = Arrangement.SpaceBetween
        ) {
            Row(verticalAlignment = Alignment.CenterVertically) {
                Spacer(modifier = Modifier.width(8.dp))
                if (currentSong.id.isNotEmpty()) {
                    Column {
                        Row(verticalAlignment = Alignment.CenterVertically) {
                            Icon(
                                painter = painterResource(id = R.drawable.ktv_ic_music),
                                contentDescription = null,
                                tint = Color.White,
                                modifier = Modifier.size(16.dp)
                            )
                            Text(currentSong.name, color = Color.White)
                            Icon(
                                Icons.AutoMirrored.Filled.KeyboardArrowRight,
                                contentDescription = "",
                                tint = Color.White,
                                modifier = Modifier.size(16.dp)
                            )
                        }
                        Text(
                            musicPlayText,
                            color = Color.White
                        )
                    }
                }
            }
            if (isAnchor) {
                val selectedColor = remember {
                    Brush.linearGradient(
                        colors = arrayListOf(
                            Color(0xFFFF88DD),
                            Color(0xFF7D00BD)
                        )
                    )
                }
                val noSelectedColor = remember {
                    Brush.linearGradient(
                        colors = arrayListOf(
                            Color.Transparent,
                            Color.Transparent
                        )
                    )
                }
                Row(
                    modifier = Modifier
                        .padding(end = 10.dp)
                        .clip(RoundedCornerShape(16.dp))
                        .background(Color(0x33FFFFFF))
                        .padding(2.dp),
                    horizontalArrangement = Arrangement.SpaceBetween
                ) {
                    Box(
                        modifier = Modifier
                            .clip(RoundedCornerShape(16.dp))
                            .background(
                                if (isAccompany) selectedColor else noSelectedColor
                            )
                            .clickable { viewModel.karaokeStore.switchAccompany(true) }
                            .padding(horizontal = 8.dp, vertical = 6.dp)
                    ) {
                        Text(
                            text = stringResource(R.string.ktv_accompany),
                            color = if (isAccompany) Color.White else Color.White.copy(alpha = 0.7f)
                        )
                    }
                    Spacer(modifier = Modifier.width(8.dp))
                    Box(
                        modifier = Modifier
                            .clip(RoundedCornerShape(16.dp))
                            .background(
                                if (!isAccompany) selectedColor else noSelectedColor
                            )
                            .clickable { viewModel.karaokeStore.switchAccompany(false) }
                            .padding(horizontal = 8.dp, vertical = 6.dp)
                    ) {
                        Text(
                            text = stringResource(R.string.ktv_origin),
                            color = if (!isAccompany) Color.White else Color.White.copy(alpha = 0.7f)
                        )
                    }
                }
            }
        }
    }
}
