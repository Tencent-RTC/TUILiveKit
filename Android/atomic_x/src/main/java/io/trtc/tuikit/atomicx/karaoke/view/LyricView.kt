package io.trtc.tuikit.atomicx.karaoke.view

import android.content.Context
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Path
import android.graphics.Typeface
import android.util.TypedValue
import android.view.View
import androidx.core.content.ContextCompat
import com.tencent.trtc.TXChorusMusicPlayer
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.LyricAlign

val TXChorusMusicPlayer.TXLyricLine.fullContent: String
    get() = characterArray.joinToString("") { it.utf8Character }

class LyricView(
    context: Context,
    private val mStore: KaraokeStore,
) : View(context) {
    private var mCurrentProgressMs: Long = 0L
    private var mCurrentLineIndex: Int = 0
    private var mHighlightProgress: Float = 0f
    private var mHighlightTextSizeSp: Float = 14f
    private var mNextLineTextSizeSp: Float = 10f
    private var mLineSpace: Float = spToPx(mNextLineTextSizeSp) * 2.0f
    private val mColorBlue = ContextCompat.getColor(context, R.color.karaoke_lyric_blue)
    private val mColorWhite = ContextCompat.getColor(context, R.color.karaoke_white)
    private val mColorGrey = ContextCompat.getColor(context, R.color.karaoke_lyric_grey)
    private var mLyricAlign: LyricAlign = LyricAlign.RIGHT

    private val mPaintCurrentLine = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = mColorWhite
        textAlign = Paint.Align.RIGHT
        textSize = spToPx(mHighlightTextSizeSp)
        typeface = Typeface.DEFAULT_BOLD
    }
    private val mPaintHighlightedLine = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = mColorBlue
        textAlign = Paint.Align.RIGHT
        textSize = spToPx(mHighlightTextSizeSp)
        typeface = Typeface.DEFAULT_BOLD
    }
    private val mPaintNextLine = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = mColorGrey
        textAlign = Paint.Align.RIGHT
        textSize = spToPx(mNextLineTextSizeSp)
        typeface = Typeface.DEFAULT
    }

    init {
        updatePaintAlign()
        updatePaintTextSize()
    }

    fun setLyricAlign(align: LyricAlign) {
        if (mLyricAlign != align) {
            mLyricAlign = align
            updatePaintAlign()
            invalidate()
        }
    }

    private fun updatePaintAlign() {
        val align = when (mLyricAlign) {
            LyricAlign.RIGHT -> Paint.Align.RIGHT
            LyricAlign.CENTER -> Paint.Align.CENTER
        }
        mPaintCurrentLine.textAlign = align
        mPaintHighlightedLine.textAlign = align
        mPaintNextLine.textAlign = align
    }

    fun setLyricTextSize(highlightSp: Float, nextLineSp: Float) {
        mHighlightTextSizeSp = highlightSp
        mNextLineTextSizeSp = nextLineSp
        updatePaintTextSize()
        invalidate()
    }

    private fun updatePaintTextSize() {
        mPaintCurrentLine.textSize = spToPx(mHighlightTextSizeSp)
        mPaintHighlightedLine.textSize = spToPx(mHighlightTextSizeSp)
        mPaintNextLine.textSize = spToPx(mNextLineTextSizeSp)
        mLineSpace = spToPx(mNextLineTextSizeSp) * 2.0f
    }

    fun spToPx(sp: Float): Float {
        return TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_SP,
            sp,
            context.resources.displayMetrics
        )
    }

    fun setPlayProgress(progressMs: Long) {
        mCurrentProgressMs = progressMs
        mStore.songLyrics.value?.let { mLyricList ->
            if (mLyricList.isEmpty()) return@let
            val newIndex = mLyricList.indexOfLast { it.startTimeMs <= progressMs }
            mCurrentLineIndex = if (newIndex != -1) newIndex else 0
            mHighlightProgress = if (mCurrentLineIndex in mLyricList.indices) {
                calcCurrentLineProgress(progressMs, mLyricList[mCurrentLineIndex])
            } else 0f
        }
        invalidate()
    }

    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)
        val mLyricList = mStore.songLyrics.value ?: return
        val mViewWidth = width.toFloat()
        val mViewHeight = height.toFloat()
        val mLineY = mViewHeight / 2
        val mNextLineY = mLineY + mLineSpace
        val mTextX = when (mLyricAlign) {
            LyricAlign.RIGHT -> mViewWidth
            LyricAlign.CENTER -> mViewWidth / 2
        }

        if (mCurrentLineIndex in mLyricList.indices) {
            val mCurrentLine = mLyricList[mCurrentLineIndex]
            val mCurrentLineText = mCurrentLine.fullContent
            canvas.drawText(mCurrentLineText, mTextX, mLineY, mPaintCurrentLine)
            val mCurrentLineTextWidth = mPaintCurrentLine.measureText(mCurrentLineText)
            val mHighlightWidth = mCurrentLineTextWidth * mHighlightProgress
            canvas.save()
            val mClipPath = Path()
            when (mLyricAlign) {
                LyricAlign.RIGHT -> {
                    mClipPath.addRect(
                        mTextX - mCurrentLineTextWidth, mLineY - mPaintCurrentLine.textSize,
                        mTextX - mCurrentLineTextWidth + mHighlightWidth, mLineY + 10f, Path.Direction.CW
                    )
                }
                LyricAlign.CENTER -> {
                    mClipPath.addRect(
                        mTextX - mCurrentLineTextWidth / 2, mLineY - mPaintCurrentLine.textSize,
                        mTextX - mCurrentLineTextWidth / 2 + mHighlightWidth, mLineY + 10f, Path.Direction.CW
                    )
                }
            }
            canvas.clipPath(mClipPath)
            canvas.drawText(mCurrentLineText, mTextX, mLineY, mPaintHighlightedLine)
            canvas.restore()
        }
        val mNextLineIndex = mCurrentLineIndex + 1
        if (mNextLineIndex in mLyricList.indices) {
            val mNextLine = mLyricList[mNextLineIndex]
            canvas.drawText(mNextLine.fullContent, mTextX, mNextLineY, mPaintNextLine)
        }
    }

    companion object {
        fun calcCurrentLineProgress(
            currentTimeMillis: Long,
            txLyricLine: TXChorusMusicPlayer.TXLyricLine,
        ): Float {
            val mWords = txLyricLine.characterArray
            val mOffsetTime = currentTimeMillis - txLyricLine.startTimeMs
            if (mWords.isEmpty()) return 1f
            val mLastWord = mWords.last()
            val mTotalDuration = mLastWord.startTimeMs + mLastWord.durationMs
            var mProgress = 0f
            if (mOffsetTime < mTotalDuration) {
                for (i in mWords.indices) {
                    val mCurrentWord = mWords[i]
                    if (mOffsetTime >= mCurrentWord.startTimeMs && mOffsetTime <= mCurrentWord.startTimeMs + mCurrentWord.durationMs) {
                        val progressBefore = i / mWords.size.toFloat()
                        val percent = 1 / mWords.size.toFloat()
                        val progressCurrent =
                            (mOffsetTime - mCurrentWord.startTimeMs) / mCurrentWord.durationMs.toFloat()
                        mProgress = progressBefore + progressCurrent * percent
                        break
                    } else if (i < mWords.size - 1) {
                        val mNextWord = mWords[i + 1]
                        if (mOffsetTime > mCurrentWord.startTimeMs + mCurrentWord.durationMs && mOffsetTime < mNextWord.startTimeMs) {
                            mProgress = (i + 1) / mWords.size.toFloat()
                        }
                    }
                }
            } else {
                mProgress = 1f
            }
            return mProgress
        }
    }
}