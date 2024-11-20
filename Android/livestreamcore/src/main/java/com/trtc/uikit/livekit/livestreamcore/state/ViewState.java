package com.trtc.uikit.livekit.livestreamcore.state;

import com.trtc.uikit.livekit.livestreamcore.view.LiveStreamView;

import java.util.HashMap;
import java.util.Map;

public class ViewState {
    public LiveStreamView              localLiveView;
    public Map<String, LiveStreamView> remoteLiveViewMap = new HashMap<>();

    public void reset() {
        localLiveView = null;
        remoteLiveViewMap.clear();
    }
}
