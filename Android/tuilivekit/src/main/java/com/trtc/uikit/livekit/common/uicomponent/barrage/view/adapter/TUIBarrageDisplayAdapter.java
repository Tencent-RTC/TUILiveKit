package com.trtc.uikit.livekit.common.uicomponent.barrage.view.adapter;

import android.view.ViewGroup;
import androidx.recyclerview.widget.RecyclerView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;

public interface TUIBarrageDisplayAdapter {
    RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType);

    void onBindViewHolder(RecyclerView.ViewHolder holder, TUIBarrage barrage);

    int getItemViewType(int position, TUIBarrage barrage);
}
