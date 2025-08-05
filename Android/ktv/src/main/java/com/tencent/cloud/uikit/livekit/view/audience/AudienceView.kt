package com.tencent.cloud.uikit.livekit.view.audience

import SeatGridView
import android.app.Activity
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
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.LiveStatus
import com.tencent.cloud.uikit.livekit.karaoke.view.MusicPlayerPanel
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.utils.ToastUtils
import com.tencent.cloud.uikit.livekit.view.common.BackgroundView
import com.tencent.cloud.uikit.livekit.view.common.bottombar.BottomMenuView
import com.tencent.cloud.uikit.livekit.view.common.topbar.StreamInfoView

@Composable
fun AudienceView(liveId: String) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val liveStatus by viewModel.karaokeState.liveStatus.collectAsState()
    when (liveStatus) {
        LiveStatus.LIVE -> {
            val ownerId =
                viewModel.karaokeStore.getVoiceRoomManager().coreState.roomState.ownerInfo?.value?.userId ?: ""
            val contextInfo = ContextInfo(liveId, ownerId, TUIRoomEngine.getSelfInfo().userId)
            viewModel.setContextInfo(contextInfo)
        }
        LiveStatus.ENDED -> {
            val context = LocalContext.current
            ToastUtils.showToast(context, context.getString(R.string.ktv_live_has_stop))
            (context as? Activity)?.finish()
        }
        else -> Unit
    }

    Box(modifier = Modifier.fillMaxSize()) {
        BackgroundView(modifier = Modifier.fillMaxSize())
        AudienceTopBar(
            liveId = liveId
        )
        Column(
            modifier = Modifier
                .align(Alignment.TopCenter)
                .padding(top = 25.dp)
        ) {
            MusicPlayerPanel(
                modifier = Modifier
                    .align(Alignment.CenterHorizontally)
                    .padding(top = 100.dp),
                liveId = liveId,
                false
            )
            Spacer(modifier = Modifier.height(20.dp))
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.Center,
                modifier = Modifier.fillMaxWidth()
            ) {
                SeatGridView(
                    liveId = liveId,
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(start = 18.dp)
                )
            }
        }
        BottomMenuView(
            liveId = liveId,
            modifier = Modifier
                .align(Alignment.BottomStart)
        )
    }
}

@Composable
fun AudienceTopBar(
    liveId: String,
    modifier: Modifier = Modifier
) {
    Box(
        modifier = modifier
            .fillMaxWidth()
            .padding(top = 44.dp)
    ) {
        StreamInfoView(
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(start = 20.dp),
            liveId = liveId
        )
        LeaveLiveView(
            modifier = Modifier
                .align(Alignment.TopEnd)
                .padding(end = 20.dp)
                .size(48.dp),
            liveId = liveId
        )
    }
}

@Composable
fun LeaveLiveView(
    modifier: Modifier = Modifier,
    liveId: String
) {
    val context = LocalContext.current
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    IconButton(
        onClick = {
            viewModel.karaokeStore.leaveLive()
            (context as? Activity)?.finish()
        },
        modifier = modifier
    ) {
        Icon(
            imageVector = Icons.Default.Close,
            tint = Color.White,
            contentDescription = "End Live",
            modifier = Modifier.size(32.dp)
        )
    }
}