package com.trtc.uikit.livekit.component.gift.view.animation.manager

import android.util.Log
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.LiveKitLogger.Companion.getComponentLogger
import com.trtc.uikit.livekit.component.gift.viewmodel.GiftModel

class GiftAnimationManager {
    private val logger: LiveKitLogger = getComponentLogger("GiftAnimationManager")
    private val giftPrepareList: MutableList<GiftModel> = ArrayList()
    private var animationPlayer: AnimationPlayer? = null
    private var isPlaying = false

    fun setPlayer(player: AnimationPlayer?) {
        animationPlayer = player
        animationPlayer?.setCallback { error: Int -> this.onFinished(error) }
    }

    fun add(gift: GiftModel) {
        if (gift.isFromSelf) {
            if (giftPrepareList.isEmpty()) {
                giftPrepareList.add(gift)
            } else {
                var lastIndex = 0
                for (i in giftPrepareList.size - 1 downTo 1) {
                    val giftModel = giftPrepareList.get(i)
                    if (giftModel.isFromSelf) {
                        lastIndex = i
                        break
                    }
                }
                giftPrepareList.add(lastIndex + 1, gift)
            }
        } else {
            giftPrepareList.add(gift)
        }
        if (giftPrepareList.size == MAX_CACHE_SIZE + 1) {
            var removeIndex = 1
            for (i in giftPrepareList.indices) {
                val giftModel = giftPrepareList[i]
                if (!giftModel.isFromSelf) {
                    removeIndex = i
                    break
                }
            }
            giftPrepareList.removeAt(removeIndex)
        }
        if (giftPrepareList.size == 1 && !isPlaying) {
            preparePlay(giftPrepareList.removeAt(0))
        }
    }

    fun startPlay(model: GiftModel) {
        logger.info( "startPlay:${model.gift?.resourceURL},isPlaying:$isPlaying")
        animationPlayer?.startPlay(model)
    }

    fun stopPlay() {
        logger.info( "startPlay:isPlaying:$isPlaying")
        isPlaying = false
        animationPlayer?.stopPlay()
    }

    private fun preparePlay(model: GiftModel) {
        logger.info( "preparePlay:${model.gift?.resourceURL},isPlaying:$isPlaying")
        isPlaying = true
        animationPlayer?.preparePlay(model)
    }

    private fun onFinished(error: Int) {
        Log.i(TAG, "onFinished:$error")
        isPlaying = false
        if (!giftPrepareList.isEmpty()) {
            preparePlay(giftPrepareList.removeAt(0))
        }
    }

    companion object {
        private const val TAG = "GiftAnimationManager"
        private const val MAX_CACHE_SIZE = 3
    }
}