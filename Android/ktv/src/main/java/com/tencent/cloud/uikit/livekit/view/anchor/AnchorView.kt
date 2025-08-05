package com.tencent.cloud.uikit.livekit.view.anchor

import android.app.Activity
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.LiveStatus
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.view.anchor.preview.AnchorPreviewPanel
import com.tencent.cloud.uikit.livekit.view.anchor.pushing.AnchorLivingView
import com.tencent.cloud.uikit.livekit.view.common.BackgroundView

@Composable
fun AnchorView(
    liveId: String,
    onStartLive: () -> Unit = {},
) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val liveStatus by viewModel.karaokeState.liveStatus.collectAsState()
    Box(modifier = Modifier.fillMaxSize()) {
        BackgroundView(modifier = Modifier.fillMaxSize())
        when (liveStatus) {
            LiveStatus.LIVE -> {
                val ownerId =
                    viewModel.karaokeStore.getVoiceRoomManager().coreState.roomState.ownerInfo?.value?.userId ?: ""
                val contextInfo = ContextInfo(liveId, ownerId, TUIRoomEngine.getSelfInfo().userId)
                viewModel.setContextInfo(contextInfo)
                AnchorLivingView(
                    liveId = liveId,
                    modifier = Modifier.fillMaxSize()
                )
            }

            else -> {
                AnchorPreviewPanel(
                    liveId = liveId,
                    onStartLive = onStartLive,
                    modifier = Modifier.fillMaxSize()
                )
            }
        }
    }
}

@Composable
fun EndLiveView(
    modifier: Modifier = Modifier
) {
    val context = LocalContext.current
    IconButton(
        onClick = { (context as? Activity)?.finish() },
        modifier = modifier
    ) {
        Icon(
            painter = painterResource(id = R.drawable.ktv_return_arrow),
            contentDescription = "Back",
            tint = Color.White,
            modifier = Modifier.size(25.dp)
        )
    }
}