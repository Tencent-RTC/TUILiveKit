package com.tencent.cloud.uikit.livekit.view.common.topbar

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.livedata.observeAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.lifecycle.viewmodel.compose.viewModel
import coil.compose.AsyncImage
import com.tencent.cloud.uikit.livekit.KTVViewModel

@Composable
fun StreamInfoView(
    modifier: Modifier = Modifier,
    liveId: String
) {
    Box(
        modifier = modifier
            .width(152.dp)
            .height(40.dp)
            .background(Color(0x9922262E), shape = RoundedCornerShape(20.dp))
    ) {
        StreamInfoContent(liveId = liveId)
    }
}

@Composable
private fun StreamInfoContent(liveId: String) {
    Row(
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier.fillMaxSize()
    ) {
        val viewModel = viewModel(KTVViewModel::class, key = liveId)
        val ownerInfo by viewModel.karaokeStore.getVoiceRoomManager().coreState.roomState.ownerInfo.observeAsState()
        Spacer(modifier = Modifier.width(8.dp))
        LiveOwnerAvatar(avatarUrl = ownerInfo?.avatarUrl)
        Spacer(modifier = Modifier.width(8.dp))
        LiveOwnerInfo(
            userName = ownerInfo?.userName?.takeIf { it.isNotEmpty() }
                ?: ownerInfo?.userId.toString(),
            modifier = Modifier.weight(1f)
        )
    }
}

@Composable
private fun LiveOwnerAvatar(avatarUrl: String?) {
    AsyncImage(
        model = avatarUrl,
        contentDescription = "Avatar",
        modifier = Modifier
            .size(32.dp)
            .clip(CircleShape)
    )
}

@Composable
private fun LiveOwnerInfo(
    userName: String,
    modifier: Modifier = Modifier
) {
    Column(
        verticalArrangement = Arrangement.Center,
        modifier = modifier
    ) {
        Text(
            text = userName,
            fontSize = 14.sp,
            color = Color.White,
            fontWeight = FontWeight.Medium,
            maxLines = 1
        )
    }
}