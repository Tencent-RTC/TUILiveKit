package com.trtc.uikit.livekit.livestreamcore.manager.module;

import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

public class ViewManager extends BaseManager {

    public ViewManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
    }

    public void updateViewLayoutInCdnMode(String layout) {
        mVideoLiveState.viewState.viewLayoutInCdnMode.set(layout);
    }

    @Override
    public void destroy() {
        mVideoLiveState.viewState.reset();
    }
}
