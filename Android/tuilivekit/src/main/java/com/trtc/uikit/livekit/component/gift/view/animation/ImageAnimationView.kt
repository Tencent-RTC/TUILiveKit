package com.trtc.uikit.livekit.component.gift.view.animation

import android.content.Context
import android.util.AttributeSet
import android.widget.LinearLayout
import com.trtc.uikit.livekit.component.gift.view.GiftBulletFrameLayout

class ImageAnimationView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    LinearLayout(context, attrs) {
    private val childViewCount = 3
    private val childViews: Array<GiftBulletFrameLayout> = arrayOfNulls<GiftBulletFrameLayout>(childViewCount) as Array<GiftBulletFrameLayout>
    private var callback: Callback? = null

    init {
        orientation = VERTICAL
        for (i in childViews.indices) {
            val bullet = GiftBulletFrameLayout(context)
            bullet.setCallback(object : GiftBulletFrameLayout.Callback {
                override fun onFinished(error: Int) {
                    checkFinished()
                }
            })
            childViews[i] = bullet
            addView(bullet)
        }
    }

    fun playAnimation(model: GiftImageAnimationInfo) {
        if (isAttachedToWindow) {
            for (bullet in childViews) {
                if (bullet.visibility === INVISIBLE) {
                    bullet.setGiftInfo(model)
                    bullet.startAnimation()
                    break
                }
            }
            checkFinished()
        } else {
            postDelayed(Runnable { this.checkFinished() }, 500)
        }
    }

    fun stopPlay() {
        for (bullet in childViews) {
            bullet.stopPlay()
        }
    }

    private fun checkFinished() {
        for (bullet in childViews) {
            if (bullet.visibility === INVISIBLE) {
                if (callback != null) {
                    callback!!.onFinished(0)
                }
                break
            }
        }
    }

    fun setCallback(callback: Callback?) {
        this@ImageAnimationView.callback = callback
    }

    interface Callback {
        fun onFinished(error: Int)
    }

    class GiftImageAnimationInfo {
        var senderAvatarUrl: String? = null
        var senderName: String? = null
        var giftName: String? = null
        var giftImageUrl: String? = null
        var giftCount: Int = 0

        init {
            reset()
        }

        fun reset() {
            senderAvatarUrl = ""
            senderName = ""
            giftName = ""
            giftImageUrl = ""
            giftCount = 0
        }
    }
}