package com.trtc.uikit.livekit.component.gift.view.animation

import android.content.Context
import android.util.AttributeSet
import android.widget.FrameLayout

abstract class AnimationView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    FrameLayout(context, attrs) {
    var callback: Callback? = null
    var roomId: String = ""
    abstract fun playAnimation(playUrl: String)
    abstract fun stopPlay()
    interface Callback {
        fun onFinished(error: Int)
    }
}