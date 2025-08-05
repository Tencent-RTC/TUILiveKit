package com.tencent.cloud.uikit.livekit.view.anchor.pushing

import SeatGridView
import android.app.Activity
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.livekit.karaoke.view.MusicPlayerPanel
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.view.common.bottombar.BottomMenuView
import com.tencent.cloud.uikit.livekit.view.common.topbar.StreamInfoView

@Composable
fun AnchorLivingView(
    modifier: Modifier = Modifier,
    liveId: String
) {
    val context = LocalContext.current
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val ownerId =
        viewModel.karaokeStore.getVoiceRoomManager().coreState.roomState.ownerInfo?.value?.userId
            ?: ""
    val contextInfo = ContextInfo(liveId, ownerId, TUIRoomEngine.getSelfInfo().userId)
    viewModel.setContextInfo(contextInfo)
    Box(modifier = modifier) {
        MusicPlayerPanel(
            modifier = Modifier
                .align(Alignment.TopCenter)
                .padding(top = 90.dp),
            liveId = liveId,
            isAnchor = true
        )
        Row(
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(start = 20.dp, end = 20.dp, top = 44.dp)
        ) {
            StreamInfoView(liveId = liveId)
            Spacer(modifier = Modifier.weight(1F))
            EndLiveView(liveId = liveId)
        }

        BottomMenuView(
            liveId = liveId,
            modifier = Modifier
                .align(Alignment.BottomEnd)
        )
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center,
            modifier = Modifier
                .fillMaxWidth()
                .padding(top = 330.dp)
                .align(Alignment.TopCenter)
        ) {
            SeatGridView(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(start = 18.dp),
                liveId = liveId
            )
        }
    }
}

@Composable
fun EndLiveView(
    liveId: String,
    modifier: Modifier = Modifier
) {
    val context = LocalContext.current
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    IconButton(
        onClick = {
            viewModel.karaokeStore.endLive()
            (context as? Activity)?.finish()
        },
        modifier = modifier.size(40.dp)
    ) {
        Icon(
            imageVector = Icons.Default.Close,
            tint = Color.White,
            contentDescription = "End Live",
            modifier = modifier.size(32.dp)
        )
    }
}
