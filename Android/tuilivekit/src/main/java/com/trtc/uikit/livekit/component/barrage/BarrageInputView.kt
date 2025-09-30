package com.trtc.uikit.livekit.component.barrage

import android.annotation.SuppressLint
import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.ui.BasicView
import com.trtc.uikit.livekit.component.barrage.view.BarrageSendView
import io.trtc.tuikit.atomicxcore.api.BarrageStore
import io.trtc.tuikit.atomicxcore.api.LiveEndedReason
import io.trtc.tuikit.atomicxcore.api.LiveListListener
import io.trtc.tuikit.atomicxcore.api.LiveListStore

@SuppressLint("ViewConstructor")
class BarrageInputView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private var barrageSendView: BarrageSendView? = null
    private var barrageStore: BarrageStore? = null

    private val liveListListener = object : LiveListListener() {
        override fun onLiveEnded(liveId: String, reason: LiveEndedReason, message: String) {
            barrageSendView?.dismiss()
        }
    }

    init {
        LayoutInflater.from(context).inflate(R.layout.livekit_barrage_view_send, this)
    }

    override fun init(roomId: String) {
        this.roomId = roomId
        initView()
    }

    override fun initStore() {
        barrageStore = BarrageStore.create(roomId)
    }

    private fun initView() {
        barrageSendView = BarrageSendView(context, roomId)
        setOnClickListener {
            barrageSendView?.also { view ->
                if (!view.isShowing) {
                    view.show(false)
                }
            }
        }
        findViewById<View>(R.id.rl_emoticons).setOnClickListener {
            barrageSendView?.also { view ->
                if (!view.isShowing) {
                    view.show(true)
                }
            }
        }
    }

    override fun addObserver() {
        LiveListStore.shared().addLiveListListener(liveListListener)
    }

    override fun removeObserver() {
        LiveListStore.shared().removeLiveListListener(liveListListener)
    }
}
