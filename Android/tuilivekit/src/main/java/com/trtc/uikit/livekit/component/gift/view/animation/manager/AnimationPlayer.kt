package com.trtc.uikit.livekit.component.gift.view.animation.manager

import com.trtc.uikit.livekit.component.gift.viewmodel.GiftModel

abstract class AnimationPlayer {
    abstract fun preparePlay(model: GiftModel)

    abstract fun startPlay(model: GiftModel)

    abstract fun stopPlay()

    abstract fun setCallback(callback: PlayCallback?)

    fun interface PlayCallback {
        fun onFinished(error: Int)
    }
}