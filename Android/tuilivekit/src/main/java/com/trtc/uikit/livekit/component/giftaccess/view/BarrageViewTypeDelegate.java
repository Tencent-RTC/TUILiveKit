package com.trtc.uikit.livekit.component.giftaccess.view;

import com.trtc.uikit.livekit.component.barrage.view.adapter.BarrageItemTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants;

import org.jetbrains.annotations.NotNull;

import io.trtc.tuikit.atomicxcore.api.Barrage;

public class BarrageViewTypeDelegate implements BarrageItemTypeDelegate {

    @Override
    public int getItemType(int position, @NotNull Barrage barrage) {
        if (barrage.getExtensionInfo().containsKey(GiftConstants.GIFT_VIEW_TYPE)) {
            String viewTypeString = String.valueOf(barrage.getExtensionInfo().get(GiftConstants.GIFT_VIEW_TYPE));
            if (String.valueOf(GiftConstants.GIFT_VIEW_TYPE_1).equals(viewTypeString)) {
                return GiftConstants.GIFT_VIEW_TYPE_1;
            }
        }
        return 0;
    }
}
