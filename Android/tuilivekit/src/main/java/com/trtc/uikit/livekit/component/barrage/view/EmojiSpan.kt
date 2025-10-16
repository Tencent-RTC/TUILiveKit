package com.trtc.uikit.livekit.component.barrage.view

import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.drawable.Drawable
import android.text.style.ImageSpan
import androidx.core.graphics.withSave

class EmojiSpan @JvmOverloads constructor(
    drawable: Drawable,
    private val emojiTranslateY: Int = 0
) : ImageSpan(drawable) {

    override fun getSize(
        paint: Paint,
        text: CharSequence,
        start: Int,
        end: Int,
        fm: Paint.FontMetricsInt?
    ) = drawable.bounds.right.also {
        fm?.run {
            val fontHeight = paint.fontMetricsInt.run { bottom - top }
            val drHeight = drawable.bounds.bottom
            val centerY = drHeight / 2 - fontHeight / 4
            ascent = -centerY
            top = -centerY
            bottom = centerY
            descent = centerY
        }
    }

    override fun draw(
        canvas: Canvas,
        text: CharSequence,
        start: Int,
        end: Int,
        x: Float,
        top: Int,
        y: Int,
        bottom: Int,
        paint: Paint
    ) {
        canvas.run {
            withSave {
                translate(x, ((bottom - top - drawable.bounds.bottom) / 2 + top - emojiTranslateY).toFloat())
                drawable.draw(this)
            }
        }
    }
}
