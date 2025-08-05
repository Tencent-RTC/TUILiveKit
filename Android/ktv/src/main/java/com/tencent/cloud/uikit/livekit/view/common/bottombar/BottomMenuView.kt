package com.tencent.cloud.uikit.livekit.view.common.bottombar

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.livedata.observeAsState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.songpicker.SongPickerPanel
import com.tencent.cloud.uikit.livekit.KTVViewModel

@Composable
fun BottomMenuView(
    liveId: String,
    modifier: Modifier = Modifier
) {
    Row(
        modifier = modifier
            .fillMaxWidth()
            .padding(bottom = 20.dp, end = 20.dp),
        horizontalArrangement = Arrangement.End,
        verticalAlignment = Alignment.Bottom
    ) {
        MicStatusView(liveId = liveId)
        Spacer(modifier = Modifier.width(10.dp))
        SongPickerPanelStartButton(liveId)
    }
}

@Composable
fun MicStatusView(
    liveId: String,
    modifier: Modifier = Modifier
) {
    val selfUserId = TUIRoomEngine.getSelfInfo().userId
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val seatList by viewModel.karaokeStore.getVoiceRoomManager().coreState.seatState.seatList.observeAsState(
        initial = emptyList()
    )

    val isOnSeat = seatList.any { it.userId == selfUserId }
    if (!isOnSeat) return
    val isMicrophoneMuted by viewModel.karaokeState.isMicrophoneMuted.collectAsState()
    val iconRes = if (isMicrophoneMuted) R.drawable.ktv_mute_audio else R.drawable.ktv_open_audio
    Box(
        modifier = modifier
            .size(40.dp)
            .clickable {
                if (isMicrophoneMuted) {
                    viewModel.karaokeStore.unMuteLocalAudio()
                } else {
                    viewModel.karaokeStore.muteLocalAudio()
                }
            },
        contentAlignment = Alignment.Center
    ) {
        Icon(
            painter = painterResource(id = iconRes),
            contentDescription = "Mic Status",
            tint = Color.Unspecified,
            modifier = Modifier.size(40.dp)
        )
    }
}

@Composable
fun SongPickerPanelStartButton(liveId: String) {
    var showDialog by remember { mutableStateOf(false) }
    if (showDialog) {
        SongPickerPanel(liveId = liveId, onDismissRequest = { showDialog = false })
    }
    Row(
        modifier = Modifier
            .width(95.dp)
            .height(40.dp)
            .clip(RoundedCornerShape(20.dp))
            .background(
                Brush.linearGradient(
                    colors = arrayListOf(
                        Color(0xFFFF88DD),
                        Color(0xFF7D00BD)
                    )
                )
            )
            .clickable { showDialog = true },
        horizontalArrangement = Arrangement.Center,
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(
            painterResource(R.drawable.ktv_song_picker_starter_icon),
            contentDescription = "",
            tint = Color.White
        )
        Spacer(modifier = Modifier.width(5.dp))
        Text(stringResource(R.string.ktv_choose_song), color = Color.White)
    }
}