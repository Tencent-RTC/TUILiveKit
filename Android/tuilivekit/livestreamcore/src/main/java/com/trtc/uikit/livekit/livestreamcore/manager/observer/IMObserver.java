package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import com.tencent.imsdk.v2.V2TIMSDKListener;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;

public class IMObserver extends V2TIMSDKListener {
    private final String mTag = "IMObserver[" + hashCode() + "]";

    protected LiveStreamManager mVideoLiveManager;

    public IMObserver(LiveStreamManager manager) {
        mVideoLiveManager = manager;
    }

    @Override
    public void onConnectSuccess() {
        Logger.info(mTag + " onConnectSuccess");
        mVideoLiveManager.getCoGuestManager().onConnectSuccess();
    }
}
