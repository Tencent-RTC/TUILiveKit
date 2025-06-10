package com.trtc.uikit.livekit.livestream.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.lang.ref.WeakReference;
import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("LiveListManagerObserver");

    protected WeakReference<LiveStreamManager> mLiveManager;

    public LiveListManagerObserver(LiveStreamManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        LOGGER.info(hashCode() + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        LiveStreamManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getRoomManager().onLiveInfoChanged(liveInfo, modifyFlagList);
        }
    }
}
