package com.trtc.uikit.livekit.features.audiencecontainer.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class RoomState {
    public TUIRoomDefine.RoomInfo   roomInfo    = new TUIRoomDefine.RoomInfo();
    public MutableLiveData<Boolean> videoStreamIsLandscape = new MutableLiveData<>(false);
}
