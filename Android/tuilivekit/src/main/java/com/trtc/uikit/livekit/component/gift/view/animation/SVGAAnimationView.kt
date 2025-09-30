package com.trtc.uikit.livekit.component.gift.view.animation

import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.util.Log
import com.opensource.svgaplayer.SVGACallback
import com.opensource.svgaplayer.SVGAImageView
import com.opensource.svgaplayer.SVGAParser
import com.opensource.svgaplayer.SVGAParser.Companion.shareParser
import com.opensource.svgaplayer.SVGAParser.ParseCompletion
import com.opensource.svgaplayer.SVGAVideoEntity
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.component.gift.service.GiftConstants
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.InputStream

class SVGAAnimationView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    AnimationView(context, attrs), SVGACallback {
    private val svgaParser: SVGAParser
    private val imageView: SVGAImageView = SVGAImageView(context)

    init {
        val params = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        addView(imageView, params)
        imageView.loops = 1
        imageView.callback = this
        svgaParser = shareParser()
        svgaParser.init(context)
    }

    override fun playAnimation(playUrl: String) {
        val stream = openInputStream(playUrl)
        if (stream == null) {
            Log.e(TAG, "InputStream is null")
            callback?.onFinished(-1)
            return
        }
        svgaParser.decodeFromInputStream(stream, "", object : ParseCompletion {
            override fun onComplete(videoItem: SVGAVideoEntity) {
                imageView.setVisibility(VISIBLE)
                imageView.setVideoItem(videoItem)
                imageView.startAnimation()
            }

            override fun onError() {
                Log.e(TAG, "decodeFromURL onError")
                callback?.onFinished(-1)
            }
        }, true, null, "")
        reportData()
    }

    override fun stopPlay() {
        imageView.stopAnimation(true)
    }

    override fun onFinished() {
        imageView.setVisibility(GONE)
        callback?.onFinished(0)
    }

    override fun onPause() {
    }

    override fun onRepeat() {
    }

    override fun onStep(frame: Int, percentage: Double) {
    }

    private fun openInputStream(path: String): InputStream? {
        try {
            val file = File(path)
            if (file.exists()) {
                return FileInputStream(file)
            }
        } catch (e: FileNotFoundException) {
            Log.i(TAG, " " + e.getLocalizedMessage())
        }
        return null
    }

    private fun reportData() {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_")
        var key = GiftConstants.DATA_REPORT_LIVE_GIFT_SVGA_PLAY_COUNT
        if (isVoiceRoom) {
            key = GiftConstants.DATA_REPORT_VOICE_GIFT_SVGA_PLAY_COUNT
        }
        reportEventData(key)
    }

    companion object {
        private const val TAG = "SVGAAnimationView"
    }
}