package com.tencent.cloud.uikit.livekit.karaoke.view.lyric

import android.util.Log
import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

class LyricsFileReader {
    companion object {
        private const val TAG = "LyricsFileReader"
        private val TIME_PATTERN = """(\d{2}):(\d{2}):(\d{2}).(\d{3})""".toRegex()
        private val WORD_PATTERN = """\<(\d+),(\d+),(\d+)\>""".toRegex()
    }

    fun parseLyricInfo(path: String): LyricInfo? {
        val lyricFile = File(path).takeIf { it.exists() && it.length() > 0 } ?: run {
            Log.w(TAG, "Lyric file not found or empty: $path")
            return null
        }

        return runCatching {
            FileInputStream(lyricFile).use { input ->
                BufferedReader(InputStreamReader(input)).use { reader ->
                    val lineList = buildList {
                        var line: String?
                        while (reader.readLine().also { line = it } != null) {
                            line?.takeIf { TIME_PATTERN.containsMatchIn(it) }?.let { timeLine ->
                                val lyricsLineInfo = parseLyricTimeLine(timeLine)
                                val lyricString = reader.readLine()
                                val updatedLineInfo = parseLyricWords(lyricString, lyricsLineInfo)
                                add(updatedLineInfo)
                            }
                        }
                    }
                    LyricInfo(lineList)
                }
            }
        }.onFailure { e ->
            Log.e(TAG, "Failed to parse lyric file: ${e.message}", e)
        }.getOrNull()
    }

    private fun parseLyricTimeLine(lineString: String): LineInfo {
        val (startTime, endTime) = lineString.split(" --> ").map { dateToMilliseconds(it) }
        return LineInfo(
            content = "",
            start = startTime,
            end = endTime,
            duration = endTime - startTime
        )
    }

    private fun parseLyricWords(lineString: String?, lineInfo: LineInfo): LineInfo {
        lineString ?: return lineInfo

        val wordMatches = WORD_PATTERN.findAll(lineString).toList()
        val words = lineString.split(WORD_PATTERN)

        val wordInfoList = wordMatches.mapIndexed { index, matchResult ->
            WordInfo(
                offset = matchResult.groupValues[1].toLong(),
                duration = matchResult.groupValues[2].toLong(),
                word = words.getOrNull(index + 1) ?: ""
            )
        }

        val content = wordInfoList.joinToString("") { it.word }

        return lineInfo.copy(
            wordList = wordInfoList,
            content = content
        )
    }

    private fun dateToMilliseconds(inputString: String): Long {
        return TIME_PATTERN.matchEntire(inputString)?.let { match ->
            match.groupValues.let { groups ->
                groups[1].toLong() * 3600000L +
                        groups[2].toLong() * 60000 +
                        groups[3].toLong() * 1000 +
                        groups[4].toLong()
            }
        } ?: run {
            Log.e(TAG, "Invalid time format: $inputString")
            -1
        }
    }
}