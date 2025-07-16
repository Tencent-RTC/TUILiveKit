package com.trtc.uikit.livekit.component.gift.store.model;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class TUIGiftData {
    public int                         giftCount;
    public TUILiveGiftManager.GiftInfo giftInfo;
    public TUIRoomDefine.UserInfo      sender;
}
