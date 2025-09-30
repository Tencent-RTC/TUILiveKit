package com.trtc.uikit.livekit.component.gift

import android.annotation.SuppressLint
import android.content.Context
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.ImageView
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.ErrorLocalized.Companion.onError
import com.trtc.uikit.livekit.common.LiveKitLogger.Companion.getComponentLogger
import com.trtc.uikit.livekit.common.ui.BasicView
import io.trtc.tuikit.atomicxcore.api.CompletionHandler
import io.trtc.tuikit.atomicxcore.api.LikeStore

@SuppressLint("ViewConstructor")
class LikeButton @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0) :
    BasicView(context, attrs, defStyleAttr) {
    private var likePendingCount = 0
    private var likeLastSendTimeMS: Long = 0
    private var hasPostLikeTask = false
    private var likeStore: LikeStore? = null
    private val mainHandler = Handler(Looper.getMainLooper())
    private val sendLikeTask = Runnable {
        LOGGER.info("sendLikeTask run")
        hasPostLikeTask = false
        sendLike(likePendingCount, System.currentTimeMillis())
    }

    init {
        LayoutInflater.from(context).inflate(R.layout.gift_layout_like_button, this)
        initView()
    }

    override fun initStore() {
        likeStore = LikeStore.create(roomId)
    }

    override fun addObserver() {}

    override fun removeObserver() {}

    private fun initView() {
        setOnClickListener { v: View? -> onLikeClick() }
    }

    fun setImageResource(resId: Int) {
        val imageLike = findViewById<ImageView?>(R.id.iv_like)
        imageLike?.setImageResource(resId)
    }

    fun onLikeClick() {
        if (TextUtils.isEmpty(roomId)) {
            LOGGER.warn("roomId is null")
            return
        }
        likePendingCount++
        val now = System.currentTimeMillis()
        val elapsed = now - likeLastSendTimeMS
        if (likeLastSendTimeMS == 0L || elapsed > LIKE_SEND_INTERVAL_MS) {
            mainHandler.removeCallbacks(sendLikeTask)
            hasPostLikeTask = false
            sendLike(likePendingCount, now)
        } else {
            if (!hasPostLikeTask) {
                mainHandler.postDelayed(sendLikeTask, LIKE_SEND_INTERVAL_MS.toLong())
                hasPostLikeTask = true
            }
        }
    }

    private fun sendLike(count: Int, now: Long) {
        likePendingCount = 0
        likeLastSendTimeMS = now
        likeStore?.sendLike(count, object : CompletionHandler {
            override fun onSuccess() {}

            override fun onFailure(code: Int, desc: String) {
                onError(TUICommonDefine.Error.fromInt(code))
                likePendingCount += count
            }
        })
    }

    companion object {
        private val LOGGER = getComponentLogger("LikeButton")
        private const val LIKE_SEND_INTERVAL_MS = 6 * 1000
    }
}