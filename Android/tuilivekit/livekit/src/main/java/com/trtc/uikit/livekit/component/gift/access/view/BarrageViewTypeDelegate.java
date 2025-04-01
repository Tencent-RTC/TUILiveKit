package com.trtc.uikit.livekit.component.gift.access.view;

import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_VIEW_TYPE;
import static com.trtc.uikit.livekit.component.gift.access.service.GiftConstants.GIFT_VIEW_TYPE_1;

import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.barrage.view.adapter.BarrageItemTypeDelegate;

public class BarrageViewTypeDelegate implements BarrageItemTypeDelegate {

    @Override
    public int getItemType(int position, Barrage barrage) {
        if (barrage.extInfo != null && barrage.extInfo.containsKey(GIFT_VIEW_TYPE)) {
            String viewTypeString = String.valueOf(barrage.extInfo.get(GIFT_VIEW_TYPE));
            if (String.valueOf(GIFT_VIEW_TYPE_1).equals(viewTypeString)) {
                return GIFT_VIEW_TYPE_1;
            }
        }
        return 0;
    }
}
