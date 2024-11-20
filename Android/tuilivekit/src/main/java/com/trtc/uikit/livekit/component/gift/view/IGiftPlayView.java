package com.trtc.uikit.livekit.component.gift.view;

import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;

public interface IGiftPlayView {
    void receiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver);
}
