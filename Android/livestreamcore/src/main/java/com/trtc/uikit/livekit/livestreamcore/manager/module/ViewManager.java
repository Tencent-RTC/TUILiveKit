package com.trtc.uikit.livekit.livestreamcore.manager.module;

import android.content.Context;

import com.trtc.uikit.livekit.livestreamcore.view.LiveStreamView;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

public class ViewManager extends BaseManager {

    public ViewManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public LiveStreamView getLocalLiveView(Context context) {
        if (mVideoLiveState.viewState.localLiveView == null) {
            mVideoLiveState.viewState.localLiveView = new LiveStreamView(context);
        }
        return mVideoLiveState.viewState.localLiveView;
    }

    public void clearLocalLiveView() {
        mVideoLiveState.viewState.localLiveView = null;
    }

    public LiveStreamView getRemoteLiveViewByUserId(Context context, String userId) {
        LiveStreamView liveView = mVideoLiveState.viewState.remoteLiveViewMap.get(userId);
        if (liveView == null) {
            liveView = new LiveStreamView(context);
            mVideoLiveState.viewState.remoteLiveViewMap.put(userId, liveView);
        }
        return liveView;
    }

    public void removeRemoteView(String userId) {
        mVideoLiveState.viewState.remoteLiveViewMap.remove(userId);
    }
}
