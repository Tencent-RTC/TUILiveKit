package com.trtc.uikit.livekit.features.audiencecontainer.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager.SeatLayout;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;

public class RoomState {
    public TUILiveListManager.LiveInfo liveInfo               = new TUILiveListManager.LiveInfo();
    public MutableLiveData<Boolean>    videoStreamIsLandscape = new MutableLiveData<>(false);
    public MutableLiveData<SeatLayout> seatLayout             = new MutableLiveData<>(null);
}
