package com.trtc.uikit.livekit.component.barrage.view.util

import android.graphics.Rect
import android.view.View
import android.view.ViewTreeObserver

class OnDecorViewListener(
    private val decorView: View,
    private val listener: OnKeyboardCallback?
) : ViewTreeObserver.OnGlobalLayoutListener {

    private var screenHeight = 0
    private var currentKeyboardHeight = 0

    override fun onGlobalLayout() {
        with(Rect()) {
            decorView.getWindowVisibleDisplayFrame(this)
            screenHeight = screenHeight.takeIf { it != 0 } ?: bottom
            val newHeight = screenHeight - bottom
            
            if (currentKeyboardHeight != -1 && currentKeyboardHeight != newHeight) {
                listener?.onKeyboardHeightUpdated(newHeight)
            }
            currentKeyboardHeight = newHeight
        }
    }

    fun clear() {
        currentKeyboardHeight = -1
        screenHeight = 0
    }

    fun interface OnKeyboardCallback {
        fun onKeyboardHeightUpdated(keyboardHeight: Int)
    }
}
