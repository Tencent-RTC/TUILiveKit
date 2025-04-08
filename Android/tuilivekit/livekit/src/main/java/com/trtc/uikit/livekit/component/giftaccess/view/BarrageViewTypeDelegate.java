package com.trtc.uikit.livekit.component.giftaccess.view;

import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.barrage.view.adapter.BarrageItemTypeDelegate;
import com.trtc.uikit.livekit.component.giftaccess.service.GiftConstants;

public class BarrageViewTypeDelegate implements BarrageItemTypeDelegate {

    @Override
    public int getItemType(int position, Barrage barrage) {
        if (barrage.extInfo != null && barrage.extInfo.containsKey(GiftConstants.GIFT_VIEW_TYPE)) {
            String viewTypeString = String.valueOf(barrage.extInfo.get(GiftConstants.GIFT_VIEW_TYPE));
            if (String.valueOf(GiftConstants.GIFT_VIEW_TYPE_1).equals(viewTypeString)) {
                return GiftConstants.GIFT_VIEW_TYPE_1;
            }
        }
        return 0;
    }
}
