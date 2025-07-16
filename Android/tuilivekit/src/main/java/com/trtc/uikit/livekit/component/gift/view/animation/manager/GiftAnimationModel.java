package com.trtc.uikit.livekit.component.gift.view.animation.manager;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class GiftAnimationModel {
    public TUILiveGiftManager.GiftInfo gift;
    public int                         giftCount;
    public TUIRoomDefine.UserInfo      sender;
    public boolean                     isFromSelf = false;
}
