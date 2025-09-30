package com.trtc.uikit.livekit.component.barrage.view

import io.trtc.tuikit.atomicxcore.api.Barrage

interface IBarrageDisplayView {
    fun insertBarrages(vararg barrages: Barrage)
}
