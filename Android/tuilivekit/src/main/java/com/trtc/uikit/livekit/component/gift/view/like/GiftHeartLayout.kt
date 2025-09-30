package com.trtc.uikit.livekit.component.gift.view.like

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.drawable.BitmapDrawable
import android.util.AttributeSet
import android.view.LayoutInflater
import android.widget.RelativeLayout
import androidx.core.content.withStyledAttributes
import com.trtc.uikit.livekit.R
import java.security.SecureRandom
import androidx.core.view.size

class GiftHeartLayout(context: Context?, attrs: AttributeSet?) : RelativeLayout(context, attrs) {
    private var animator: GiftAbstractPathAnimator? = null
    private val heartViewList: MutableList<GiftHeartView> = ArrayList<GiftHeartView>()
    private var textHeight = 0
    private var bitmapHeight = 0
    private var bitmapWidth = 0
    private var pointX = 0

    private fun findViewById(context: Context?) {
        LayoutInflater.from(context).inflate(R.layout.gift_layout_heart_layout, this)
        val bitmap = BitmapFactory.decodeResource(resources, R.drawable.gift_ic_like_png)
        bitmapHeight = bitmap.getWidth()
        bitmapWidth = bitmap.getHeight()
        textHeight = sp2px(getContext(), 20f) + bitmapHeight / 2

        pointX = bitmapWidth

        bitmap.recycle()
    }

    private fun init(attrs: AttributeSet?, defStyleAttr: Int) {
        context.withStyledAttributes(attrs, R.styleable.LiveKitGiftHeartLayout, defStyleAttr, 0) {
            val mInitX = 30
            if (pointX <= mInitX && pointX >= 0) {
                pointX -= 10
            } else if (pointX >= -mInitX && pointX <= 0) {
                pointX += 10
            } else {
                pointX = mInitX
            }

            animator = GiftPathAnimator(
                GiftAbstractPathAnimator.Config.fromTypeArray(
                    this, mInitX.toFloat(), textHeight.toFloat(),
                    pointX, bitmapWidth, bitmapHeight
                )
            )
        }
    }

    override fun clearAnimation() {
        for (i in 0 until size) {
            getChildAt(i).clearAnimation()
        }
        removeAllViews()
    }

    private fun resourceLoad() {
        val hearts = arrayOfNulls<Bitmap>(mDrawableIds.size)
        mHeartsDrawable = arrayOfNulls<BitmapDrawable>(mDrawableIds.size)
        for (i in mDrawableIds.indices) {
            hearts[i] = BitmapFactory.decodeResource(resources, mDrawableIds[i])
            mHeartsDrawable[i] = BitmapDrawable(resources, hearts[i])
        }
    }

    private val mRandom = SecureRandom()
    private lateinit var mHeartsDrawable: Array<BitmapDrawable?>

    init {
        findViewById(context)
        resourceLoad()
        init(attrs, 0)
    }

    fun addFavor() {
        var heartView: GiftHeartView? = null
        for (view in heartViewList) {
            if (view.getParent() == null) {
                heartView = view
                break
            }
        }
        if (heartView == null) {
            if (heartViewList.size >= HEART_VIEW_COUNT_MAX) {
                return
            }
            heartView = GiftHeartView(getContext())
            heartView.setDrawable(mHeartsDrawable[mRandom.nextInt(8)])
            heartViewList.add(heartView)
        }
        animator?.start(heartView, this)
    }

    companion object {
        private const val HEART_VIEW_COUNT_MAX = 30

        private fun sp2px(context: Context, spValue: Float): Int {
            val fontScale = context.resources.displayMetrics.scaledDensity
            return (spValue * fontScale + 0.5f).toInt()
        }

        private val mDrawableIds = intArrayOf(
            R.drawable.gift_heart0, R.drawable.gift_heart1, R.drawable.gift_heart2,
            R.drawable.gift_heart3, R.drawable.gift_heart4, R.drawable.gift_heart5,
            R.drawable.gift_heart6, R.drawable.gift_heart7, R.drawable.gift_heart8,
        )
    }
}