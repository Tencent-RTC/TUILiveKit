package com.trtc.uikit.livekit.livestreamcore.state;

import com.trtc.tuikit.common.livedata.LiveData;

public class ViewState {

    public final LiveData<String> viewLayoutInCdnMode = new LiveData<>();

    public void reset() {
        viewLayoutInCdnMode.set(null);
    }
}
