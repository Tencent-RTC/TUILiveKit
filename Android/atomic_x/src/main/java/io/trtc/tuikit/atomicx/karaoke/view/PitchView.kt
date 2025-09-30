package io.trtc.tuikit.atomicx.karaoke.view

import android.content.Context
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Typeface
import android.graphics.drawable.Drawable
import android.os.Handler
import android.os.Looper
import android.util.TypedValue
import android.view.View
import androidx.core.content.ContextCompat
import com.tencent.trtc.TXChorusMusicPlayer
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import java.lang.ref.WeakReference
import kotlin.math.abs
import kotlin.math.cos
import kotlin.math.sin
import kotlin.random.Random

class PitchView(
    context: Context,
    private val mStore: KaraokeStore,
) : View(context) {
    private val mLineColor = ContextCompat.getColor(context, R.color.karaoke_pitch_line)
    private val mDotColor = ContextCompat.getColor(context, R.color.karaoke_white)
    private val mScoreLineColor = ContextCompat.getColor(context, R.color.karaoke_color_grey_8c)
    private val mHighlightColor = ContextCompat.getColor(context, R.color.karaoke_pitch_score_text)
    private val mScoreTextColor = ContextCompat.getColor(context, R.color.karaoke_pitch_score_text)
    private val mPitchLineGapDp = 2f
    private val mPitchLineHeightDp = 3f
    private val mPitchDotRadiusDp = 4.5f
    private val mLabelGapDp = 3f
    private val mScoreTextSizeSp = 8f
    private val mMinPitch = 2
    private val mMaxPitch = 7
    private val mBubbleHeightPx = dpToPx(12f)
    private var mPitchList: List<TXChorusMusicPlayer.TXReferencePitch> = emptyList()
    private var mPitchLineLengths: List<Float> = emptyList()
    private var mRedEndPercent: FloatArray = FloatArray(0)
    private var mScoreValue: Float = 0f
    private var mIsAnimPaused: Boolean = false
    private var mCurSegment: Int = 0
    private var mPercentInSegment: Float = 0f
    private var mAnimPitch: Float = 5f
    private var mScrollOffset: Float = 0f

    private data class Butterfly(
        val drawable: Drawable,
        val x0: Float,
        val y0: Float,
        val angle: Float,
        val scale: Float,
        val baseRotation: Float,
        val startTime: Long,
        val lifeMs: Long,
    )

    private val mButterflies = mutableListOf<Butterfly>()
    private val mButterflyDrawables: List<Drawable?> by lazy {
        listOf(ContextCompat.getDrawable(context, R.drawable.karaoke_song_well_icon))
    }
    private val mButterflyFlyDistance = dpToPx(52f)
    private val mButterflyLife = 1350L
    private var mCurSegmentEmitTime = 0L
    private var mLastSegmentForButterfly = -1
    private val mPitchLineHeightPx = dpToPx(mPitchLineHeightDp)
    private val mDrawTop: Float
        get() {
            return height / 8f + mPitchLineHeightPx / 2f
        }
    private val mDrawHeight: Float
        get() {
            return (height * 0.75f - mPitchLineHeightPx).coerceAtLeast(0f)
        }
    private val mAnimInterval = 16L
    private val mAnimHandler = Handler(Looper.getMainLooper())

    private class PitchViewAnimRunnable(view: PitchView) : Runnable {
        private val viewRef: WeakReference<PitchView> = WeakReference(view)

        override fun run() {
            val view = viewRef.get()
            if (view == null || !view.isAttachedToWindow) {
                return
            }

            view.advanceAnim()
            view.updateRedAndButterfly()
            view.updateButterflies()
            view.postInvalidateOnAnimation()
            view.mAnimHandler.postDelayed(this, view.mAnimInterval)
        }
    }
    private val mAnimRunnable = PitchViewAnimRunnable(this)

    fun setPlayProgress(progressMs: Long) {
    }

    fun setPitchList(list: List<TXChorusMusicPlayer.TXReferencePitch>) {
        mPitchList = list ?: emptyList()
        recalculatePitchLineLengths()
        mRedEndPercent = FloatArray(mPitchList.size) { 0f }
        mCurSegment = 0
        mPercentInSegment = 0f
        mScoreValue = 0f
        mCurDotY = null
        mButterflies.clear()
        invalidate()
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        mAnimHandler.removeCallbacks(mAnimRunnable)
        mAnimHandler.removeCallbacksAndMessages(null)
    }

    fun randomTestData(totalPoints: Int = 400) {
        val now = System.currentTimeMillis()
        val durationMs = 1500L
        val list = List(totalPoints) { idx ->
            val obj = TXChorusMusicPlayer.TXReferencePitch()
            obj.startTimeMs = now + idx * durationMs
            obj.durationMs = durationMs
            obj.referencePitch = Random.nextInt(3, 8)
            obj
        }
        setPitchList(list)
        mScoreValue = Random.nextInt(80, 100) / 10f
        invalidate()
    }

    private fun recalculatePitchLineLengths() {
        val n = mPitchList.size
        if (n == 0) {
            mPitchLineLengths = emptyList()
            return
        }
        mPitchLineLengths = List(mPitchList.size) { dpToPx(20f) }
    }

    private var mLastAnimTime: Long = -1
    private fun advanceAnim() {
        if (mPitchList.isEmpty()) return
        val now = System.currentTimeMillis()
        if (mLastAnimTime < 0) mLastAnimTime = now
        val dt = now - mLastAnimTime
        mLastAnimTime = now
        val durationMs = mPitchList.getOrNull(mCurSegment)?.durationMs ?: 500L
        mPercentInSegment += dt.toFloat() / durationMs
        if (mPercentInSegment > 1f) {
            mCurSegment++
            mPercentInSegment = 0f
            mLastAnimTime = -1
            if (mCurSegment >= mPitchList.size) {
                mCurSegment = mPitchList.size - 1
                mPercentInSegment = 1f
                for (i in mRedEndPercent.indices) mRedEndPercent[i] = 1f
                return
            }
            mCurSegmentEmitTime = 0L
        }
        mScrollOffset = 0f
        for (i in 0 until mCurSegment) {
            mScrollOffset += mPitchLineLengths[i] + dpToPx(mPitchLineGapDp)
        }
        mScrollOffset += mPitchLineLengths.getOrNull(mCurSegment)?.let { it * mPercentInSegment }
            ?: 0f
        mAnimPitch = mPitchList.getOrNull(mCurSegment)?.referencePitch?.toFloat() ?: 5f
    }

    private fun updateRedAndButterfly() {
        if (mPitchList.isEmpty()) return
        var totalProgress = mScrollOffset
        for (i in mPitchList.indices) {
            val lineLen = mPitchLineLengths[i]
            if (totalProgress >= lineLen) {
                mRedEndPercent[i] = 1f
                totalProgress -= (lineLen + dpToPx(mPitchLineGapDp))
            } else if (totalProgress > 0f && totalProgress < lineLen) {
                mRedEndPercent[i] = (totalProgress / lineLen).coerceIn(0f, 1f)
                totalProgress = 0f
            } else {
                mRedEndPercent[i] = 0f
            }
        }
        if (mCurSegment < 0 || mCurSegment >= mPitchList.size) return
        val lineLen = mPitchLineLengths[mCurSegment]
        val lineLeftX = getSegmentX(mCurSegment)
        val centerX = width / 2f
        if (centerX in lineLeftX..(lineLeftX + lineLen)) {
            val curPitch = mPitchList[mCurSegment].referencePitch.toFloat()
            val shouldRed = abs(mAnimPitch - curPitch) < 0.35f
            if (shouldRed) {
                val now = System.currentTimeMillis()
                if (mLastSegmentForButterfly != mCurSegment) {
                    mLastSegmentForButterfly = mCurSegment
                    mCurSegmentEmitTime = now
                    emitButterfly(mCurSegment, centerX)
                } else if (now - mCurSegmentEmitTime > 400) {
                    emitButterfly(mCurSegment, centerX)
                    mCurSegmentEmitTime = now
                }
            }
        }
    }

    private fun getSegmentX(index: Int): Float {
        var x = width / 2f
        for (j in 0 until index) {
            x += mPitchLineLengths[j] + dpToPx(mPitchLineGapDp)
        }
        return x - mScrollOffset
    }

    private fun emitButterfly(segment: Int, centerX: Float) {
        if (segment < 0 || segment >= mPitchList.size) return
        val drawable = mButterflyDrawables.filterNotNull().randomOrNull() ?: return
        val curPitch = mPitchList[segment].referencePitch
        val percent = (curPitch - mMinPitch).toFloat() / (mMaxPitch - mMinPitch)
        val lineY = mDrawTop + percent * mDrawHeight
        val startX = centerX - dpToPx(4f + 11 * Random.nextFloat())
        val startY = lineY + dpToPx((Random.nextFloat() - 0.5f) * 16f)
        val angle = 240f + (Random.nextFloat() - 0.5f) * 20f
        val scale = 1.00f + Random.nextFloat() * 0.34f
        val baseRotation = -15f + Random.nextFloat() * 30f
        mButterflies += Butterfly(
            drawable = drawable,
            x0 = startX,
            y0 = startY,
            angle = angle,
            scale = scale,
            baseRotation = baseRotation,
            startTime = System.currentTimeMillis(),
            lifeMs = mButterflyLife
        )
    }

    private fun updateButterflies() {
        val now = System.currentTimeMillis()
        mButterflies.removeAll { now - it.startTime > it.lifeMs }
    }

    private val mScoreTagImg: Drawable? by lazy {
        ContextCompat.getDrawable(context, R.drawable.karaoke_score_bg)
    }
    private val mLinePaint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        style = Paint.Style.STROKE
        strokeWidth = dpToPx(mPitchLineHeightDp)
        color = mLineColor
        strokeCap = Paint.Cap.ROUND
    }
    private val mHighlightLinePaint = Paint(mLinePaint).apply { color = mHighlightColor }
    private val mDotPaint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        style = Paint.Style.FILL
        color = mDotColor
        setShadowLayer(8f, 0f, 2f, 0x77000000)
    }
    private val mScoreLinePaint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = mScoreLineColor
        strokeWidth = dpToPx(1f)
        style = Paint.Style.STROKE
    }
    private val mScoreTextPaint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = mScoreTextColor
        style = Paint.Style.FILL
        textSize = spToPx(mScoreTextSizeSp)
        textAlign = Paint.Align.CENTER
        typeface = Typeface.create(Typeface.DEFAULT, Typeface.BOLD)
    }

    private fun dpToPx(dp: Float): Float =
        TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, context.resources.displayMetrics)

    private fun spToPx(sp: Float): Float =
        TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, sp, context.resources.displayMetrics)

    private fun getTextHeightCenterOffset(paint: Paint): Float {
        val metrics = paint.fontMetrics
        return (metrics.descent - metrics.ascent) / 2 - metrics.descent
    }

    private var mCurDotY: Float? = null

    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)
        if (mPitchList.isEmpty() || mPitchLineLengths.isEmpty()) return

        val mVertCenterX = width / 2f

        for (i in mPitchList.indices) {
            val pitchValue = mPitchList[i].referencePitch
            val lineLen = mPitchLineLengths[i]
            val x1 = getSegmentX(i)
            val x2 = x1 + lineLen
            val percent = (pitchValue - mMinPitch).toFloat() / (mMaxPitch - mMinPitch)
            val lineY = mDrawTop + percent * mDrawHeight
            val redLen = mRedEndPercent[i].coerceIn(0f, 1f) * lineLen
            if (redLen > 0f) {
                canvas.drawLine(x1, lineY, x1 + redLen, lineY, mHighlightLinePaint)
            }
            if (redLen < lineLen) {
                canvas.drawLine(x1 + redLen, lineY, x2, lineY, mLinePaint)
            }
        }

        canvas.drawLine(
            mVertCenterX,
            0f,
            mVertCenterX,
            height.toFloat(),
            mScoreLinePaint
        )

        if (mCurSegment < mPitchList.size && mCurSegment >= 0) {
            val pitchValue = mPitchList[mCurSegment].referencePitch
            val percentY = (pitchValue - mMinPitch).toFloat() / (mMaxPitch - mMinPitch)
            val targetDotY = mDrawTop + percentY * mDrawHeight
            if (mCurDotY == null) {
                mCurDotY = targetDotY
            } else {
                val animFraction = 0.18f
                mCurDotY = mCurDotY!! + (targetDotY - mCurDotY!!) * animFraction
            }
            canvas.drawCircle(mVertCenterX, mCurDotY!!, dpToPx(mPitchDotRadiusDp), mDotPaint)
            if (mStore.isScoringEnabled.value == true) {
                val tagDrawable = mScoreTagImg
                val str = "10.0"
                if (tagDrawable != null) {
                    val tagHeight = mBubbleHeightPx
                    val scale = tagHeight / tagDrawable.intrinsicHeight.toFloat()
                    val tagWidth = tagDrawable.intrinsicWidth * scale
                    val dotTop =
                        (mCurDotY!! - dpToPx(mPitchDotRadiusDp) - dpToPx(mLabelGapDp) - tagHeight).toInt()
                    tagDrawable.setBounds(
                        (mVertCenterX - tagWidth / 2).toInt(), dotTop,
                        (mVertCenterX + tagWidth / 2).toInt(), dotTop + tagHeight.toInt()
                    )
                    tagDrawable.draw(canvas)
                    val textBaseY =
                        dotTop + tagHeight / 2f + getTextHeightCenterOffset(mScoreTextPaint)
                    canvas.drawText(str, mVertCenterX, textBaseY, mScoreTextPaint)
                }
            }
        }
        drawButterflies(canvas)
    }

    private fun drawButterflies(canvas: Canvas) {
        val now = System.currentTimeMillis()
        for (b in mButterflies) {
            val t = ((now - b.startTime).toFloat() / b.lifeMs).coerceIn(0f, 1f)
            val rad = Math.toRadians(b.angle.toDouble())
            val dx = mButterflyFlyDistance * t * cos(rad).toFloat()
            val dy = mButterflyFlyDistance * t * sin(rad).toFloat()
            val x = b.x0 + dx
            val y = b.y0 + dy
            val scale = b.scale * (1.00f - 0.14f * t)
            val d = b.drawable
            val w = d.intrinsicWidth * scale
            val h = d.intrinsicHeight * scale
            val alpha = (180 * (1 - t)).toInt().coerceIn(0, 255)
            canvas.save()
            canvas.translate(x, y)
            val swing = sin(t * Math.PI * 2.0 * 1.1f).toFloat() * 18f
            canvas.rotate(b.baseRotation + swing + b.angle)
            d.setBounds((-w / 2).toInt(), (-h / 2).toInt(), (w / 2).toInt(), (h / 2).toInt())
            d.alpha = alpha
            d.draw(canvas)
            canvas.restore()
        }
    }

    fun startDemoAnim() {
        randomTestData()
        mAnimHandler.removeCallbacks(mAnimRunnable)
        mAnimHandler.post(mAnimRunnable)
        mButterflies.clear()
        mLastSegmentForButterfly = -1
        mCurSegmentEmitTime = 0L
        mLastAnimTime = -1
        mIsAnimPaused = false
    }

    fun stopDemoAnim() {
        mAnimHandler.removeCallbacks(mAnimRunnable)
        mButterflies.clear()
        mLastSegmentForButterfly = -1
        mCurSegmentEmitTime = 0L
        mLastAnimTime = -1
        mIsAnimPaused = true
    }

    fun pauseDemoAnim() {
        mIsAnimPaused = true
        mAnimHandler.removeCallbacks(mAnimRunnable)
    }

    fun resumeDemoAnim() {
        if (mIsAnimPaused) {
            mIsAnimPaused = false
            mAnimHandler.post(mAnimRunnable)
            mLastAnimTime = -1
        }
    }
}