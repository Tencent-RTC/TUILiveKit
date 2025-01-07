package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;

public class LiveLayoutManagerObserver extends TUILiveLayoutManager.Observer {

    private final String mTag = "LiveLayoutManagerObserver[" + hashCode() + "]";

    private final LiveStreamManager mLiveStreamManager;

    public LiveLayoutManagerObserver(LiveStreamManager manager) {
        mLiveStreamManager = manager;
    }

    @Override
    public void onLiveVideoLayoutChanged(String roomId, String layoutInfo) {
        Logger.info(mTag + " onLiveVideoLayoutChanged:[roomId:" + roomId + ", layoutInfo:" + layoutInfo + "]");
        if (!TextUtils.equals(mLiveStreamManager.getRoomState().roomId, roomId)) {
            return;
        }
        mLiveStreamManager.getViewManager().updateViewLayoutInCdnMode(layoutInfo);
    }
}
