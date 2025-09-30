package com.trtc.uikit.livekit.component.barrage.view.adapter

import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import io.trtc.tuikit.atomicxcore.api.Barrage

interface BarrageItemAdapter {
    fun onCreateViewHolder(parent: ViewGroup): RecyclerView.ViewHolder

    fun onBindViewHolder(holder: RecyclerView.ViewHolder, position: Int, barrage: Barrage)
}
