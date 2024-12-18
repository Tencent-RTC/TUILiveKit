package com.trtc.uikit.livekit.livestreamcore.manager.module;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

public class ViewManager extends BaseManager {

    public ViewManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
    }

    public void updateViewLayoutInCdnMode(String layout) {
        mVideoLiveState.viewState.viewLayoutInCdnMode.set(layout);
    }

    public boolean isMixUserId(String userId) {
        return userId != null && userId.startsWith("livekit_" + TUILogin.getSdkAppId() + "_feedback_");
    }

    @Override
    public void destroy() {
        mVideoLiveState.viewState.reset();
    }
}
