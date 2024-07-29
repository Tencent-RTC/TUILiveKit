package com.trtc.uikit.livekit.state.view;

import static com.trtc.uikit.livekit.state.LiveDefine.LinkStatus.NONE;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.state.LiveDefine;

public class ViewState {
    public LiveData<LiveDefine.LinkStatus> linkStatus             = new LiveData<>(NONE);
    public LiveData<LiveDefine.LiveStatus> liveStatus             = new LiveData<>(LiveDefine.LiveStatus.NONE);
    public LiveData<Boolean>               autoOpenCameraOnSeated = new LiveData<>(true);

    public void reset() {
        linkStatus.set(NONE);
        autoOpenCameraOnSeated.set(true);
        liveStatus.set(LiveDefine.LiveStatus.NONE);
    }
}
