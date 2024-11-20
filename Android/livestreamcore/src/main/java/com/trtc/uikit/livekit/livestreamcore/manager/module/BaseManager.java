package com.trtc.uikit.livekit.livestreamcore.manager.module;

import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

public abstract class BaseManager {
    protected LiveStreamState mVideoLiveState;
    protected ILiveStream     mVideoLiveService;

    protected BaseManager(LiveStreamState state, ILiveStream service) {
        mVideoLiveState = state;
        mVideoLiveService = service;
    }

    protected abstract void destroy();
}
