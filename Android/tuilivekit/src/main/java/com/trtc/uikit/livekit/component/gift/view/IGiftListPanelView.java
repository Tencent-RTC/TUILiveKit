package com.trtc.uikit.livekit.component.gift.view;

import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;

public interface IGiftListPanelView {
    void sendGift(Gift gift, int giftCount, GiftUser receiver);
}
