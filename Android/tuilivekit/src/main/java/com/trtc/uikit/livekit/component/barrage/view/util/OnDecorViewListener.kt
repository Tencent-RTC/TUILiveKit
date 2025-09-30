package com.trtc.uikit.livekit.component.barrage.view.util

import android.graphics.Rect
import android.view.View
import android.view.ViewTreeObserver

class OnDecorViewListener(
    private val mDecorView: View,
    private val mListener: OnKeyboardCallback?
) : ViewTreeObserver.OnGlobalLayoutListener {

    private var mScreenHeight = 0
    private var mCurrentKeyboardHeight = 0

    override fun onGlobalLayout() {
        Rect().apply {
            mDecorView.getWindowVisibleDisplayFrame(this)
            mScreenHeight = if (mScreenHeight == 0) bottom else mScreenHeight
            val nowHeight = mScreenHeight - bottom

            if (mCurrentKeyboardHeight != -1 && mCurrentKeyboardHeight != nowHeight) {
                mListener?.onKeyboardHeightUpdated(nowHeight)
            }
            mCurrentKeyboardHeight = nowHeight
        }
    }

    fun clear() {
        mCurrentKeyboardHeight = -1
        mScreenHeight = 0
    }

    interface OnKeyboardCallback {
        fun onKeyboardHeightUpdated(keyboardHeight: Int)
    }
}
