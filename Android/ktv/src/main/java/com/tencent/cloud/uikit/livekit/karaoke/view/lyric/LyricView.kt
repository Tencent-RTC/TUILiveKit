package com.tencent.cloud.uikit.livekit.karaoke.view.lyric

import android.graphics.Paint
import android.graphics.Path
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.tween
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.nativeCanvas
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.graphics.withSave
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.uikit.livekit.KTVViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@Composable
fun LyricComposeView(
    modifier: Modifier = Modifier,
    liveId: String,
    defaultLyricText: String = "",
    defaultTextColor: Color = Color(0xCCFFFFFF),
    highLightTextColor: Color = Color(0xFFFF8607),
    defaultTextSizeSp: Float = 14f,
    highLightTextSizeSp: Float = 20f,
    lineSpace: Float = 50f,
    scale: Int = 3
) {
    val density = LocalDensity.current
    val defaultTextSizePx = with(density) { defaultTextSizeSp.sp.toPx() }
    val highLightTextSizePx = with(density) { highLightTextSizeSp.sp.toPx() }

    var currentPlayLine by remember { mutableIntStateOf(0) }
    var highLightTextPy by remember { mutableFloatStateOf(0f) }
    var defaultTextPy by remember { mutableFloatStateOf(0f) }

    val coroutineScope = rememberCoroutineScope()
    var lyricInfoState by remember { mutableStateOf<LyricInfo?>(null) }

    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val currentSong by viewModel.songState.currentSong.collectAsState()
    val currentPlayProgress by viewModel.karaokeState.currentPlayProgress.collectAsState()

    LaunchedEffect(currentSong.lrcUrl) {
        coroutineScope.launch {
            val info = withContext(Dispatchers.IO) {
                LyricsFileReader().parseLyricInfo(currentSong.lrcUrl)
            }
            lyricInfoState = info
            viewModel.karaokeStore.generateStandardPitchModels(info)
        }
    }
    val lyricInfo = lyricInfoState

    LaunchedEffect(currentPlayProgress, lyricInfo) {
        if (lyricInfo != null) {
            val position = lyricInfo.lineList
                .take(lyricInfo.lineList.size)
                .indexOfFirst { (it.start > currentPlayProgress) }
                .takeIf { it != -1 } ?: lyricInfo.lineList.size
            currentPlayLine = position
        }
    }

    val currentProgress = if (lyricInfo != null && currentPlayLine > 0) {
        val line = lyricInfo.lineList.getOrNull(currentPlayLine - 1)
        if (line != null) {
            calculateCurrentKrcProgress(currentPlayProgress, line).coerceIn(0f, 1f)
        } else 0f
    } else 0f

    val animatedProgress = remember { Animatable(currentProgress) }
    LaunchedEffect(currentProgress) {
        if (currentProgress >= animatedProgress.value) {
            animatedProgress.animateTo(
                targetValue = currentProgress,
                animationSpec = tween(durationMillis = 200)
            )
        } else {
            animatedProgress.snapTo(currentProgress)
        }
    }

    Canvas(
        modifier = modifier
            .fillMaxWidth()
            .height(120.dp)
    ) {
        val width = size.width
        val paddingTop = 0f
        if (lyricInfo != null && lyricInfo.lineList.isNotEmpty() && currentPlayLine >= 0) {
            if ((currentPlayLine - 1 < lyricInfo.lineList.size) && (currentPlayLine - 1 >= 0)
            ) {
                val line = lyricInfo.lineList.getOrNull(currentPlayLine - 1)
                if (line != null) {
                    drawKaraokeHighLightLrcRow(
                        text = line.content,
                        progress = animatedProgress.value,
                        rowX = width * 0.5f,
                        rowY = highLightTextSizePx - highLightTextPy + paddingTop,
                        defaultTextColor = defaultTextColor,
                        highLightTextColor = highLightTextColor,
                        highLightTextSizePx = highLightTextSizePx,
                        lineSpace = lineSpace,
                        scale = scale
                    )
                }
            }
            if (currentPlayLine < lyricInfo.lineList.size) {
                val text = lyricInfo.lineList[currentPlayLine].content
                drawContext.canvas.nativeCanvas.apply {
                    val paint = Paint().apply {
                        isAntiAlias = true
                        color = defaultTextColor.toArgb()
                        textAlign = Paint.Align.CENTER
                        textSize = defaultTextSizePx
                    }
                    drawText(
                        text,
                        width * 0.5f,
                        highLightTextSizePx + lineSpace + defaultTextSizePx - defaultTextPy + paddingTop,
                        paint
                    )
                }
            }
        } else {
            drawContext.canvas.nativeCanvas.apply {
                val paint = Paint().apply {
                    isAntiAlias = true
                    color = defaultTextColor.toArgb()
                    textAlign = Paint.Align.CENTER
                    textSize = defaultTextSizePx
                }
                drawText(
                    defaultLyricText,
                    width * 0.5f,
                    highLightTextSizePx + lineSpace + defaultTextSizePx - defaultTextPy + paddingTop,
                    paint
                )
            }
        }
    }
}

fun DrawScope.drawKaraokeHighLightLrcRow(
    text: String,
    progress: Float,
    rowX: Float,
    rowY: Float,
    defaultTextColor: Color,
    highLightTextColor: Color,
    highLightTextSizePx: Float,
    lineSpace: Float,
    scale: Int
) {
    val defaultPaint = Paint().apply {
        isAntiAlias = true
        textAlign = Paint.Align.CENTER
        textSize = highLightTextSizePx
        color = defaultTextColor.toArgb()
    }

    val highLineWidth = defaultPaint.measureText(text)
    var x = rowX
    val location = progress * highLineWidth

    if (highLineWidth > rowX * 2) {
        if (location < rowX * 2 / scale) {
            x = highLineWidth / 2f
        } else {
            val offsetX = location - (rowX * 2 / scale)
            val widthGap = highLineWidth - rowX * 2
            x = if (offsetX < widthGap) {
                highLineWidth / 2f - offsetX
            } else {
                highLineWidth / 2f - widthGap
            }
        }
    }

    drawContext.canvas.nativeCanvas.drawText(text, x, rowY, defaultPaint)

    val leftOffset = x - highLineWidth / 2f
    val highWidth = (progress * highLineWidth).toInt()

    if (highWidth > 1 && (rowY * 2).toInt() > 1) {
        drawContext.canvas.nativeCanvas.withSave {
            val highlightPaint = Paint().apply {
                isAntiAlias = true
                textAlign = Paint.Align.CENTER
                textSize = highLightTextSizePx
                color = highLightTextColor.toArgb()
            }
            val path = Path().apply {
                addRect(
                    leftOffset,
                    rowY - highLightTextSizePx,
                    leftOffset + highWidth,
                    rowY + lineSpace,
                    Path.Direction.CW
                )
            }
            drawContext.canvas.nativeCanvas.clipPath(path)
            drawContext.canvas.nativeCanvas.drawText(text, x, rowY, highlightPaint)
        }
    }
}

fun calculateCurrentKrcProgress(currentTimeMillis: Long, lineInfo: LineInfo): Float {
    val words = lineInfo.wordList
    val offsetTime = currentTimeMillis - lineInfo.start
    if (words.isEmpty()) return 1f
    val lastWord = words.last()
    val allWordDuration = lastWord.offset + lastWord.duration
    var progressAll = 0f
    if (offsetTime < allWordDuration) {
        for (i in words.indices) {
            val currentWordInfo = words[i]
            if (offsetTime >= currentWordInfo.offset && offsetTime <= currentWordInfo.offset + currentWordInfo.duration) {
                val progressBefore = i / words.size.toFloat()
                val percent = 1 / words.size.toFloat()
                val progressCurrentWord = (offsetTime - currentWordInfo.offset) / currentWordInfo.duration.toFloat()
                progressAll = progressBefore + progressCurrentWord * percent
                break
            } else if (i < words.size - 1) {
                val nextWordInfo = words[i + 1]
                if (offsetTime > currentWordInfo.offset + currentWordInfo.duration && offsetTime < nextWordInfo.offset) {
                    progressAll = (i + 1) / words.size.toFloat()
                }
            }
        }
    } else {
        progressAll = 1f
    }
    return progressAll
}