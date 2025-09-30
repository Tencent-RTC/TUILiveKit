package com.trtc.uikit.livekit.voiceroomcore.view

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.util.AttributeSet
import android.view.View
import com.trtc.tuikit.common.util.ScreenUtil
import com.trtc.uikit.livekit.R

@SuppressLint("CustomViewStyleable")
class RippleView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : View(context, attrs, defStyleAttr) {

    private val paint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        strokeWidth = ScreenUtil.dip2px(2f).toFloat()
        style = Paint.Style.STROKE
        strokeCap = Paint.Cap.ROUND
    }

    private var circleWidth = 0f
    private var circleAlpha = 255
    private var startRadius = 0f

    init {
        context.obtainStyledAttributes(attrs, R.styleable.VoiceRoomCoreRippleView).apply {
            paint.color = getColor(R.styleable.VoiceRoomCoreRippleView_color, Color.BLUE)
            startRadius = getDimension(R.styleable.VoiceRoomCoreRippleView_start_radius, 0f)
            recycle()
        }
        circleWidth = startRadius
        setBackgroundColor(Color.TRANSPARENT)
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val defaultSize = ScreenUtil.dip2px(120f)
        setMeasuredDimension(
            resolveSize(defaultSize, widthMeasureSpec),
            resolveSize(defaultSize, heightMeasureSpec)
        )
    }

    override fun onDraw(canvas: Canvas) {
        paint.alpha = circleAlpha
        val centerX = width / 2f
        val centerY = height / 2f
        val maxRadius = centerX - paint.strokeWidth / 2

        canvas.drawCircle(centerX, centerY, circleWidth, paint)

        when {
            circleWidth > maxRadius -> resetCircle()
            else -> updateCircle(maxRadius)
        }
        invalidate()
    }

    private fun resetCircle() {
        circleWidth = startRadius
        circleAlpha = 255
    }

    private fun updateCircle(maxRadius: Float) {
        circleAlpha = (40 + 215 * (1 - circleWidth / maxRadius)).toInt()
        circleWidth += 1
    }
}