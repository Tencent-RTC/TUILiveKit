package com.trtc.uikit.livekit.component.beauty.tebeauty

import android.content.Context
import android.view.View
import android.widget.FrameLayout

class TEBeautyView(context: Context) : FrameLayout(context) {

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        init()
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        unInit()
    }

    private fun init() {
        TEBeautyManager.setListener(beautyListener) 
        TEBeautyManager.init(context)
    }

    private fun unInit() {
        TEBeautyManager.setListener(null)
    }

    private val beautyListener = object : TEBeautyManager.OnBeautyListener {
        override fun onCreateBeautyView(view: View) {
            view.let {
                removeAllViews()
                addView(it)
            }
        }

        override fun onDestroyBeautyView() {
            removeAllViews()
        }
    }
}
