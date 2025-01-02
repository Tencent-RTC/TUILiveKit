package com.trtc.uikit.livekit.livestream.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;

import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private final String mTag = "LiveListManagerObserver[" + hashCode() + "]";

    protected LiveStreamManager mLiveManager;

    public LiveListManagerObserver(LiveStreamManager manager) {
        mLiveManager = manager;
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        LiveStreamLog.info(mTag + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        mLiveManager.getRoomManager().onLiveInfoChanged(liveInfo, modifyFlagList);
    }
}
