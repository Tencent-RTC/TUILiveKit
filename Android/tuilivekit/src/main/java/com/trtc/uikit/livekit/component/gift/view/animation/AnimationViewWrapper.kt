package com.trtc.uikit.livekit.component.gift.view.animation

import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.util.Log
import androidx.core.view.size
import java.util.Locale

class AnimationViewWrapper @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    AnimationView(context, attrs) {
    private enum class AnimationSourceType {
        SVGA, MP4, OTHER
    }

    private var animationView: AnimationView? = null
    private var effectAnimationView: TCEffectAnimationView? = null
    private var svgaAnimationView: SVGAAnimationView? = null

    override fun playAnimation(playUrl: String) {
        val sourceType = getSourceType(playUrl)
        if (sourceType == AnimationSourceType.MP4) {
            playEffectAnimation(playUrl)
        } else if (sourceType == AnimationSourceType.SVGA) {
            playSVGAAnimation(playUrl)
        } else {
            Log.e(TAG, "not support url: $playUrl")
            callback?.onFinished(-1)
        }
    }

    private fun playEffectAnimation(url: String) {
        if (effectAnimationView == null) {
            effectAnimationView = TCEffectAnimationView(context)
        }
        if ((effectAnimationView?.size ?: 0) > 0) {
            if (animationView !== effectAnimationView) {
                switchAnimationView(effectAnimationView)
                animationView = effectAnimationView
            }
            animationView?.callback = (callback)
            animationView?.playAnimation(url)
        } else {
            Log.e(TAG, "not support TCEffectAnimationView")
            callback?.onFinished(-1)
        }
    }

    private fun playSVGAAnimation(url: String) {
        if (svgaAnimationView == null) {
            svgaAnimationView = SVGAAnimationView(context)
        }
        if (animationView != svgaAnimationView) {
            switchAnimationView(svgaAnimationView)
            animationView = svgaAnimationView
        }
        animationView?.callback = callback
        animationView?.playAnimation(url)
    }

    private fun switchAnimationView(view: AnimationView?) {
        removeAllViews()
        val params = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        addView(view, params)
    }

    private fun getSourceType(url: String): AnimationSourceType {
        if (TextUtils.isEmpty(url)) {
            return AnimationSourceType.OTHER
        }
        if (url.lowercase(Locale.getDefault()).endsWith(".mp4")) {
            return AnimationSourceType.MP4
        } else if (url.lowercase(Locale.getDefault()).endsWith(".svga")) {
            return AnimationSourceType.SVGA
        }
        return AnimationSourceType.OTHER
    }

    override fun stopPlay() {
        animationView?.stopPlay()
    }

    companion object {
        private const val TAG = "AnimationViewWrapper"
    }
}