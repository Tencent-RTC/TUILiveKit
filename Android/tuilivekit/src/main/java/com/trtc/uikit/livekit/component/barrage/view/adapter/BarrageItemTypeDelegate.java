package com.trtc.uikit.livekit.component.barrage.view.adapter;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

public interface BarrageItemTypeDelegate {
    int getItemType(int position, Barrage barrage);
}
