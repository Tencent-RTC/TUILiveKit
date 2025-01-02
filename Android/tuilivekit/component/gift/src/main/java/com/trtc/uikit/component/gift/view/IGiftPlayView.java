package com.trtc.uikit.component.gift.view;

import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;

public interface IGiftPlayView {
    void receiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver);
}
