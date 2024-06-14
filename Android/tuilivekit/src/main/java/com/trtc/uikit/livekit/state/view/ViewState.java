package com.trtc.uikit.livekit.state.view;

import static com.trtc.uikit.livekit.state.LiveDefine.LinkStatus.NONE;
import static com.trtc.uikit.livekit.state.LiveDefine.LinkType.VIDEO;
import static com.trtc.uikit.livekit.state.LiveDefine.NavigationStatus.MAIN;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.state.LiveDefine;

public class ViewState {
    public LiveData<LiveDefine.NavigationStatus> currentNavigationState = new LiveData<>(MAIN);
    public LiveData<LiveDefine.LinkStatus>       linkStatus             = new LiveData<>(NONE);
    public LiveData<LiveDefine.LinkType>         linkType               = new LiveData<>(VIDEO);
    public LiveData<Boolean>                     enableCamera           = new LiveData<>(true);
    public LiveData<LiveDefine.LiveStatus>       liveStatus             = new LiveData<>(LiveDefine.LiveStatus.NONE);
    public LiveData<Boolean>                     isPortrait             = new LiveData<>(true);

    public void reset() {
        currentNavigationState.set(MAIN);
        linkStatus.set(NONE);
        linkType.set(VIDEO);
        enableCamera.set(true);
        liveStatus.set(LiveDefine.LiveStatus.NONE);
        isPortrait.set(true);
    }
}
