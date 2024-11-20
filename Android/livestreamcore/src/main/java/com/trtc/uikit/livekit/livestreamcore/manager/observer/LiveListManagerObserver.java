package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;

import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private final String mTag = "LiveListManagerObserver[" + hashCode() + "]";

    protected LiveStreamManager mVideoLiveManager;

    public LiveListManagerObserver(LiveStreamManager manager) {
        mVideoLiveManager = manager;
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        Logger.info(mTag + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:"
                + new Gson().toJson(modifyFlagList) + "]");
    }
}
