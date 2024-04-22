package com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver;


import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;

import java.util.List;

public abstract class GiftListQuery {
    public abstract void queryGiftInfoList(IGiftCloudServer.Callback<List<TUIGift>> callback);
}
