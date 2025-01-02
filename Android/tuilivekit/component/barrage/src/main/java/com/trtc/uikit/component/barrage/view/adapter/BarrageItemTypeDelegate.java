package com.trtc.uikit.component.barrage.view.adapter;

import com.trtc.uikit.component.barrage.store.model.Barrage;

public interface BarrageItemTypeDelegate {
    int getItemType(int position, Barrage barrage);
}
