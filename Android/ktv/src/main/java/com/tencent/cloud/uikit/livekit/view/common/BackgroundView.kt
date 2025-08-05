package com.tencent.cloud.uikit.livekit.view.common

import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import com.tencent.cloud.uikit.ktv.R

@Composable
fun BackgroundView(modifier: Modifier = Modifier) {
    Image(
        painter = painterResource(id = R.drawable.ktv_room_bg),
        contentDescription = null,
        modifier = modifier,
        contentScale = ContentScale.Crop
    )
}