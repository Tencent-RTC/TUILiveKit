package com.trtc.uikit.livekit.component.beauty.tebeauty.store

import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager

object TEBeautyStore {
    @JvmStatic
    fun unInit() {
        TEBeautyManager.clear()
    }
}
