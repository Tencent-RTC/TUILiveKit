package com.trtc.uikit.livekit.component.barrage.view.adapter;

import android.view.ViewGroup;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

public interface BarrageItemAdapter {
    RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent);

    void onBindViewHolder(RecyclerView.ViewHolder holder, int position, Barrage barrage);
}
