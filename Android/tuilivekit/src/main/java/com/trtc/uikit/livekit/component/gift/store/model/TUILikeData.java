package com.trtc.uikit.livekit.component.gift.store.model;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class TUILikeData {
    public TUIRoomDefine.UserInfo sender;

    public TUILikeData(TUIRoomDefine.UserInfo sender) {
        this.sender = sender;
    }
}
