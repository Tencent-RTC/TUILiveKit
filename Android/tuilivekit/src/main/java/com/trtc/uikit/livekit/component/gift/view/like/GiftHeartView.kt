package com.trtc.uikit.livekit.component.gift.view.like

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.PorterDuff
import android.graphics.PorterDuffColorFilter
import android.graphics.drawable.BitmapDrawable
import android.util.AttributeSet
import androidx.appcompat.widget.AppCompatImageView
import androidx.core.graphics.createBitmap
import com.trtc.uikit.livekit.R

class GiftHeartView : AppCompatImageView {
    private var heartResId = R.drawable.gift_heart0
    private var heartBorderResId = R.drawable.gift_heart1

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs)

    constructor(context: Context, attrs: AttributeSet?, defStyleAttr: Int) : super(context, attrs, defStyleAttr)

    constructor(context: Context) : super(context)

    fun setDrawable(bitmap: BitmapDrawable?) {
        setImageDrawable(bitmap)
    }

    fun setColor(color: Int) {
        val heart = createHeart(color)
        heart?.let {
            setImageDrawable(BitmapDrawable(resources, heart))
        }
    }

    fun setColorAndDrawables(color: Int, heartResId: Int, heartBorderResId: Int) {
        if (heartResId != this.heartResId) {
            sHeart = null
        }
        if (heartBorderResId != this.heartBorderResId) {
            sHeartBorder = null
        }
        this.heartResId = heartResId
        this.heartBorderResId = heartBorderResId
        setColor(color)
    }

    fun createHeart(color: Int): Bitmap? {
        if (sHeart == null) {
            sHeart = BitmapFactory.decodeResource(resources, heartResId)
        }
        if (sHeartBorder == null) {
            sHeartBorder = BitmapFactory.decodeResource(resources, heartBorderResId)
        }

        val heartBorder: Bitmap? = sHeartBorder
        val bm: Bitmap? = createBitmapSafely(heartBorder!!.getWidth(), heartBorder.getHeight())
        if (bm == null) {
            return null
        }
        val canvas: Canvas = sCanvas
        canvas.setBitmap(bm)
        val p: Paint = sPaint
        canvas.drawBitmap(heartBorder, 0f, 0f, p)
        p.setColorFilter(PorterDuffColorFilter(color, PorterDuff.Mode.SRC_ATOP))

        val heart: Bitmap? = sHeart
        val dx = (heartBorder.getWidth() - heart!!.getWidth()) / 2f
        val dy = (heartBorder.getHeight() - heart.getHeight()) / 2f
        canvas.drawBitmap(heart, dx, dy, p)
        p.setColorFilter(null)
        canvas.setBitmap(null)
        return bm
    }

    companion object {
        private val sPaint = Paint(Paint.ANTI_ALIAS_FLAG or Paint.FILTER_BITMAP_FLAG)
        private val sCanvas = Canvas()

        private var sHeart: Bitmap? = null
        private var sHeartBorder: Bitmap? = null

        private fun createBitmapSafely(width: Int, height: Int): Bitmap? {
            try {
                return createBitmap(width, height, Bitmap.Config.ARGB_8888)
            } catch (error: OutOfMemoryError) {
                error.printStackTrace()
            }
            return null
        }
    }
}