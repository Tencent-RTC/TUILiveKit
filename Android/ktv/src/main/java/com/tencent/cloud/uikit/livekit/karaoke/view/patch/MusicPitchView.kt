package com.tencent.cloud.uikit.livekit.karaoke.view.patch

import androidx.compose.animation.core.Spring
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.animation.core.spring
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.CornerRadius
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.MusicPitchModel

@Composable
fun MusicPitchView(
    modifier: Modifier = Modifier,
    standardPitchModels: List<MusicPitchModel>,
    currentProgress: Long,
    currentPitch: Int,
) {
    val defaultPitchColor = Color(0xFFB67CCC)
    val verticalLineColor = Color(0xFF9548B5)
    val indicatorColor = Color.White
    val screenScrollPeriod = 4000f
    val currentPitchPositionX = 0.3f

    val lastProgress = remember { mutableStateOf(currentProgress.toFloat()) }
    val animatedProgress by animateFloatAsState(
        targetValue = currentProgress.toFloat(),
        animationSpec = spring(stiffness = Spring.StiffnessLow)
    )
    val animatedPitch by animateFloatAsState(
        targetValue = currentPitch.toFloat(),
        animationSpec = spring(stiffness = Spring.StiffnessLow)
    )
    LaunchedEffect(currentProgress) {
        lastProgress.value = currentProgress.toFloat()
    }

    val bgPainter = painterResource(id = R.drawable.ktv_bg_pitch)

    Box(modifier = modifier.background(Color.Transparent)) {
        Image(
            painter = bgPainter,
            contentDescription = null,
            modifier = Modifier.fillMaxSize(),
            contentScale = ContentScale.FillBounds
        )
        Canvas(modifier = Modifier.fillMaxSize()) {
            val width = size.width
            val height = size.height
            val ratioOfPixelToTimeMS = width / screenScrollPeriod
            val pitchItemHeightPx = 8f
            val indicatorRadius = 10f
            val lineX = currentPitchPositionX * width
            drawLine(
                color = verticalLineColor,
                start = Offset(lineX, 0f),
                end = Offset(lineX, height),
                strokeWidth = 2f
            )
            standardPitchModels.forEach { pitch ->
                val left =
                    width * currentPitchPositionX + pitch.startTime * ratioOfPixelToTimeMS - animatedProgress * ratioOfPixelToTimeMS
                val right = left + pitch.duration * ratioOfPixelToTimeMS
                val safeLeft = left.coerceAtLeast(0f)
                val safeRight = right.coerceAtMost(width)
                if (safeRight > safeLeft) {
                    val top = ((100 - pitch.pitch) / 100f) * (height - pitchItemHeightPx) + pitchItemHeightPx
                    val rect = Rect(
                        left = safeLeft,
                        top = top,
                        right = safeRight,
                        bottom = top + pitchItemHeightPx
                    )
                    drawRoundRect(
                        color = defaultPitchColor,
                        topLeft = Offset(rect.left, rect.top),
                        size = Size(rect.width, rect.height),
                        cornerRadius = CornerRadius(rect.height / 2, rect.height / 2)
                    )
                }
            }

            val safeLineX = lineX.coerceIn(indicatorRadius, width - indicatorRadius)
            val indicatorY = ((100 - animatedPitch) / 100f) * (height - pitchItemHeightPx) + pitchItemHeightPx
            val safeIndicatorY = indicatorY.coerceIn(indicatorRadius, height - indicatorRadius)
            drawCircle(
                color = indicatorColor,
                radius = indicatorRadius,
                center = Offset(safeLineX, safeIndicatorY)
            )
        }
    }
}

