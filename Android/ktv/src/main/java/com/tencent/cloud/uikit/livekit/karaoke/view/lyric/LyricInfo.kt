package com.tencent.cloud.uikit.livekit.karaoke.view.lyric

import androidx.compose.runtime.Immutable

@Immutable
data class LyricInfo(
    val lineList: List<LineInfo> = emptyList()
)

@Immutable
data class LineInfo(
    val content: String,
    val start: Long,
    val end: Long,
    val duration: Long,
    val wordList: List<WordInfo> = emptyList()
)

@Immutable
data class WordInfo(
    val offset: Long,
    val duration: Long,
    val word: String
)
