package com.tencent.cloud.uikit.livekit.view.anchor.preview

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.lifecycle.viewmodel.compose.viewModel
import coil3.compose.AsyncImage
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.KTVViewModel

val COLOR_BRUSH_DIALOG_CONTENT = Brush.horizontalGradient(
    colors = listOf(Color(0xFF0B0023), Color(0xFF271A25))
)
val COLOR_BLUE = Color(0xFF1C66E5)
val COVER_URL_LIST = listOf<String>(
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover2.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover3.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover4.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover5.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover6.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover7.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover8.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover9.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover10.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover11.png",
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover12.png",
)

@Composable
fun CoverPicker(liveId: String, onDismissRequest: () -> Unit) {
    Dialog(
        properties = DialogProperties(
            usePlatformDefaultWidth = false,
            dismissOnClickOutside = true
        ),
        onDismissRequest = onDismissRequest
    ) {
        Column {
            Spacer(
                modifier = Modifier
                    .fillMaxWidth()
                    .weight(1F)
                    .clickable(onClick = onDismissRequest)
            )
            CoverPickerContent(liveId, onDismissRequest)
        }
    }
}

@Composable
fun CoverPickerContent(liveId: String, onDismissRequest: () -> Unit) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .height(500.dp)
            .clip(RoundedCornerShape(topStart = 15.dp, topEnd = 15.dp))
            .background(COLOR_BRUSH_DIALOG_CONTENT)
    ) {
        // Top
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .padding(10.dp),
            contentAlignment = Alignment.Center
        ) {
            Row(
                modifier = Modifier
                    .matchParentSize()
                    .clickable(true, onClick = {
                        onDismissRequest()
                    })
            ) {
                Icon(
                    Icons.AutoMirrored.Filled.KeyboardArrowLeft,
                    contentDescription = "",
                    tint = Color.White
                )
            }
            Text(stringResource(R.string.ktv_cover), color = Color.White, fontSize = 16.sp)
        }

        // Grid
        var selectedIndex by remember {
            val url = viewModel.anchorViewStore.getAnchorState().coverUrl.value
            val index = COVER_URL_LIST.indexOf(url)
            mutableIntStateOf(index.coerceIn(0, COVER_URL_LIST.size - 1))
        }
        LazyVerticalGrid(
            columns = GridCells.Fixed(3),
            modifier = Modifier
                .fillMaxWidth()
                .weight(1F)
                .padding(10.dp)
        ) {
            items(COVER_URL_LIST.size) { index ->
                Box(
                    modifier = Modifier
                        .padding(4.dp)
                        .aspectRatio(1f)
                        .background(Color.Transparent)
                        .border(
                            width = if (selectedIndex == index) 3.dp else 0.dp,
                            color = if (selectedIndex == index) COLOR_BLUE else Color.Transparent,
                            shape = RoundedCornerShape(6.dp)
                        )
                        .clickable(true, onClick = {
                            selectedIndex = index
                        }),
                    contentAlignment = Alignment.Center
                ) {
                    AsyncImage(
                        model = COVER_URL_LIST[index],
                        contentDescription = "Live Cover",
                        contentScale = ContentScale.Crop,
                        modifier = Modifier.clip(RoundedCornerShape(6.dp)),
                        error = painterResource(R.drawable.ktv_room_cover),
                    )
                    if (selectedIndex == index) {
                        Box(
                            modifier = Modifier
                                .size(36.dp)
                                .padding(8.dp)
                                .background(Color.White, shape = CircleShape)
                                .align(Alignment.BottomEnd)
                        ) {
                            Icon(
                                Icons.Default.CheckCircle,
                                contentDescription = "",
                                tint = COLOR_BLUE
                            )
                        }
                    }
                }
            }
        }
        Button(
            modifier = Modifier
                .fillMaxWidth()
                .padding(10.dp),
            onClick = {
                viewModel.anchorViewStore.setCoverUrl(COVER_URL_LIST[selectedIndex])
                onDismissRequest()
            },
            colors = ButtonDefaults.buttonColors(containerColor = COLOR_BLUE)
        ) {
            Text(
                stringResource(R.string.ktv_set_as_cover),
                color = Color.White,
            )
        }
    }
}