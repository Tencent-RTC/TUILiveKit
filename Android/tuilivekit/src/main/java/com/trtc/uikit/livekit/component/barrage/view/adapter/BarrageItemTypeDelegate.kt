package com.trtc.uikit.livekit.component.barrage.view.adapter

import io.trtc.tuikit.atomicxcore.api.Barrage

interface BarrageItemTypeDelegate {
    fun getItemType(position: Int, barrage: Barrage): Int
}
