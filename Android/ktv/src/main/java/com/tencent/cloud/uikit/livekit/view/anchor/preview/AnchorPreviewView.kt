package com.tencent.cloud.uikit.livekit.view.anchor.preview

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.colorResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.LiveStatus
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.view.anchor.EndLiveView

@Composable
fun AnchorPreviewPanel(
    liveId: String,
    onStartLive: () -> Unit,
    modifier: Modifier = Modifier
) {
    Box(modifier = modifier) {
        EndLiveView(
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(start = 12.dp, top = 44.dp)
        )
        LiveInfoEditView(
            liveId = liveId,
            modifier = Modifier
                .align(Alignment.TopCenter)
                .padding(top = 70.dp)
        )
        StartLiveView(
            liveId = liveId,
            onStartLive = onStartLive,
            modifier = Modifier
                .align(Alignment.BottomCenter)
                .padding(bottom = 56.dp, start = 32.dp, end = 32.dp)
                .fillMaxWidth()
                .height(48.dp)
        )
    }
}

@Composable
fun StartLiveView(
    liveId: String,
    onStartLive: () -> Unit,
    modifier: Modifier = Modifier
) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val liveStatus by viewModel.karaokeState.liveStatus.collectAsState()
    val buttonColor = colorResource(id = R.color.ktv_design_standard_b1)
    Button(
        onClick = onStartLive,
        enabled = liveStatus != LiveStatus.LIVE,
        modifier = modifier,
        colors = ButtonDefaults.buttonColors(containerColor = buttonColor)
    ) {
        Text(
            text = stringResource(id = R.string.ktv_create_stream),
            fontSize = 16.sp
        )
    }
}
