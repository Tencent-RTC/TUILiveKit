package com.trtc.uikit.livekit.common.ui

import android.content.Context
import android.graphics.*
import android.util.AttributeSet
import android.widget.FrameLayout
import androidx.annotation.Nullable
import com.tencent.qcloud.tuicore.util.ScreenUtil

class RoundFrameLayout @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null
) : FrameLayout(context, attrs) {
    private var radius = ScreenUtil.dip2px(12f)
    private var rect: RectF? = null
    private val path = Path()

    init {
        setWillNotDraw(false)
    }

    fun setRadius(radius: Int) {
        this.radius = radius
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        rect = RectF(0f, 0f, w.toFloat(), h.toFloat())
    }

    override fun draw(canvas: Canvas) {
        path.reset()
        rect?.let { path.addRoundRect(it, radius.toFloat(), radius.toFloat(), Path.Direction.CW) }
        canvas.save().also { saveCount ->
            canvas.clipPath(path, Region.Op.INTERSECT)
            super.draw(canvas)
            canvas.restoreToCount(saveCount)
        }
    }
}