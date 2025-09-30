package com.trtc.uikit.livekit.component.gift.view

import android.animation.Animator
import android.animation.AnimatorListenerAdapter
import android.content.Context
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import android.util.AttributeSet
import android.util.Log
import android.view.LayoutInflater
import android.view.animation.DecelerateInterpolator
import android.view.animation.OvershootInterpolator
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.RelativeLayout
import android.widget.TextView
import androidx.constraintlayout.utils.widget.ImageFilterView
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.gift.view.animation.ImageAnimationView.GiftImageAnimationInfo

class GiftBulletFrameLayout @JvmOverloads constructor(private val mContext: Context, attrs: AttributeSet? = null) :
    FrameLayout(
        mContext, attrs
    ) {
    private val handler = Handler(Looper.getMainLooper())
    private val layoutInflater: LayoutInflater = LayoutInflater.from(mContext)
    private var giftEndAnimationRunnable: Runnable? = null
    private var giftGroup: RelativeLayout? = null
    private var imageGiftIcon: ImageFilterView? = null
    private var imageSendUserIcon: ImageView? = null
    private var textSendUserName: TextView? = null
    private var textGiftTitle: TextView? = null
    private var callback: Callback? = null
    private val giftImageAnimationInfo = GiftImageAnimationInfo()

    init {
        val rootView = layoutInflater.inflate(R.layout.gift_layout_bullet, this)
        giftGroup = rootView.findViewById(R.id.gift_group)
        imageGiftIcon = rootView.findViewById(R.id.iv_gift_icon)
        imageSendUserIcon = rootView.findViewById(R.id.iv_send_user_icon)
        textSendUserName = rootView.findViewById(R.id.tv_send_user_name)
        textGiftTitle = rootView.findViewById(R.id.tv_gift_title)
        visibility = INVISIBLE
    }

    fun setGiftInfo(info: GiftImageAnimationInfo) {
        giftImageAnimationInfo.senderAvatarUrl = info.senderAvatarUrl
        giftImageAnimationInfo.senderName = info.senderName
        giftImageAnimationInfo.giftCount = info.giftCount
        giftImageAnimationInfo.giftName = info.giftName
        giftImageAnimationInfo.giftImageUrl = info.giftImageUrl
        textSendUserName?.text = giftImageAnimationInfo.senderName
        textGiftTitle?.text = giftImageAnimationInfo.giftName
    }

    fun stopPlay() {
        visibility = INVISIBLE
        if (giftEndAnimationRunnable != null) {
            handler.removeCallbacks(giftEndAnimationRunnable!!)
        }
        giftImageAnimationInfo.reset()
    }

    private fun initLayoutState() {
        if (!isAttachedToWindow) {
            Log.w("GiftBulletFrameLayout", "initLayoutState: isAttachedToWindow is false")
            return
        }
        this.visibility = VISIBLE
        if (!TextUtils.isEmpty(giftImageAnimationInfo.giftImageUrl)) {
            imageGiftIcon?.let {
                ImageLoader.loadImage(
                    mContext,
                    imageGiftIcon!!,
                    giftImageAnimationInfo.giftImageUrl,
                    R.drawable.gift_default_avatar
                )
            }
        }
        if (!TextUtils.isEmpty(giftImageAnimationInfo.senderAvatarUrl)) {
            imageSendUserIcon?.let {
                ImageLoader.loadImage(
                    mContext,
                    imageSendUserIcon!!,
                    giftImageAnimationInfo.senderAvatarUrl,
                    R.drawable.gift_default_avatar
                )
            }
        }
    }

    fun startAnimation() {
        visibility = VISIBLE
        imageGiftIcon?.setVisibility(VISIBLE)
        val duration = 400
        val giftLayoutAnimator = AnimationUtils.createFadesInFromLtoR(
            giftGroup, -getWidth().toFloat(), 0f, duration, OvershootInterpolator()
        )
        giftLayoutAnimator.addListener(object : AnimatorListenerAdapter() {
            override fun onAnimationStart(animation: Animator) {
                initLayoutState()
            }
        })

        val giftImageAnimator = AnimationUtils.createFadesInFromLtoR(
            imageGiftIcon, -getWidth().toFloat(), 0f, duration, DecelerateInterpolator()
        )
        giftImageAnimator.addListener(object : AnimatorListenerAdapter() {
            override fun onAnimationStart(animation: Animator) {
                imageGiftIcon?.setVisibility(VISIBLE)
            }
        })
        AnimationUtils.startAnimation(giftLayoutAnimator, giftImageAnimator)
        giftEndAnimationRunnable = Runnable { this.endAnimation() }
        giftEndAnimationRunnable?.let {
            handler.postDelayed(giftEndAnimationRunnable!!, GIFT_DISMISS_TIME.toLong())
        }
    }

    fun endAnimation() {
        val fadeAnimator = AnimationUtils.createFadesOutAnimator(
            this, 0f, -100f, 500, 0
        )
        val fadeAnimator2 = AnimationUtils.createFadesOutAnimator(
            this, 100f, 0f, 0, 0
        )
        fadeAnimator2.addListener(object : AnimatorListenerAdapter() {
            override fun onAnimationEnd(animation: Animator) {
                visibility = INVISIBLE
                setAlpha(1f)
                callback?.onFinished(0)
            }
        })
        AnimationUtils.startAnimation(fadeAnimator, fadeAnimator2)
    }

    fun setCallback(callback: Callback?) {
        this.callback = callback
    }

    interface Callback {
        fun onFinished(error: Int)
    }

    companion object {
        private const val GIFT_DISMISS_TIME = 3000
    }
}