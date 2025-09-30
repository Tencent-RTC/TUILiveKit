package com.trtc.uikit.livekit.common.ui

import android.content.Context
import android.util.AttributeSet
import android.widget.FrameLayout
import kotlinx.coroutines.Job

abstract class BasicView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {
    protected var subscribeStateJob: Job? = null
    protected var roomId: String = ""

    open fun init(roomId: String) {
        if (roomId.isEmpty()) {
            return
        }
        this.roomId = roomId
        setupLifecycleIfNeeded()
    }

    protected abstract fun initStore()

    protected abstract fun addObserver()

    protected abstract fun removeObserver()

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        if (roomId.isEmpty()) {
            return
        }
        setupLifecycleIfNeeded()
    }

    override fun onDetachedFromWindow() {
        removeObserver()
        super.onDetachedFromWindow()
    }

    private fun setupLifecycleIfNeeded() {
        if (roomId.isEmpty()) {
            return
        }
        initStore()
        removeObserver()
        addObserver()
    }
}
