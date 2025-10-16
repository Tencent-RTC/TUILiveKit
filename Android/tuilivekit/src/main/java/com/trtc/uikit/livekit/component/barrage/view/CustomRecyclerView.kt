package com.trtc.uikit.livekit.component.barrage.view

import android.content.Context
import android.util.AttributeSet
import android.view.GestureDetector
import android.view.MotionEvent
import androidx.recyclerview.widget.RecyclerView

class CustomRecyclerView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : RecyclerView(context, attrs, defStyleAttr) {

    private val gestureDetector = GestureDetector(context, object : GestureDetector.SimpleOnGestureListener() {
        override fun onLongPress(e: MotionEvent) {
            if (inTouch) {
                isLongPressed = true
            }
        }
    })

    var isLongPressed = false
        private set

    private var inTouch = false

    override fun dispatchTouchEvent(event: MotionEvent): Boolean {
        updateLongPressedState(event)
        handleInterceptTouchEvent()
        return super.dispatchTouchEvent(event)
    }

    private fun updateLongPressedState(event: MotionEvent) {
        when (event.action) {
            MotionEvent.ACTION_DOWN -> inTouch = true
            MotionEvent.ACTION_UP, MotionEvent.ACTION_CANCEL -> {
                inTouch = false
                isLongPressed = false
            }
        }
    }

    private fun handleInterceptTouchEvent() {
        if (!canScrollVertically(-1) && !canScrollVertically(1)) return
        parent.requestDisallowInterceptTouchEvent(true)
    }

    override fun onInterceptTouchEvent(event: MotionEvent): Boolean {
        return gestureDetector.onTouchEvent(event) || super.onInterceptTouchEvent(event)
    }
}
