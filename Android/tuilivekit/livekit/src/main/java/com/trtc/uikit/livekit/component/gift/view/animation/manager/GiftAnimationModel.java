package com.trtc.uikit.livekit.component.gift.view.animation.manager;

import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;

public class GiftAnimationModel {
    public Gift     gift;
    public int      giftCount;
    public GiftUser sender;
    public boolean  isFromSelf = false;
}
