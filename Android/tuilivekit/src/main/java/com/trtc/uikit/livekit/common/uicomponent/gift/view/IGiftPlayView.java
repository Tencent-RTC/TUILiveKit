package com.trtc.uikit.livekit.common.uicomponent.gift.view;

import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;

public interface IGiftPlayView {
    void receiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver);
}
