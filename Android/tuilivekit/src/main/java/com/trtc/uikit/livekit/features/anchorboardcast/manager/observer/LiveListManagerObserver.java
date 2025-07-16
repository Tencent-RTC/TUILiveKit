package com.trtc.uikit.livekit.features.anchorboardcast.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.lang.ref.WeakReference;
import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("LiveListManagerObserver");

    protected WeakReference<AnchorManager> mLiveManager;

    public LiveListManagerObserver(AnchorManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        LOGGER.info(hashCode() + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getRoomManager().onLiveInfoChanged(liveInfo, modifyFlagList);
        }
    }
}
