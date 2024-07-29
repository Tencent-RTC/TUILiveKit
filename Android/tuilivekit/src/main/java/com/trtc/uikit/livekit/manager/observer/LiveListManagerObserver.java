package com.trtc.uikit.livekit.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;

import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private final String mTag = "LiveListManagerObserver[" + hashCode() + "]";

    protected LiveController mLiveController;

    public LiveListManagerObserver(LiveController liveController) {
        mLiveController = liveController;
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        LiveKitLog.info(mTag + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        mLiveController.getRoomController().onLiveInfoChanged(liveInfo, modifyFlagList);
    }
}
