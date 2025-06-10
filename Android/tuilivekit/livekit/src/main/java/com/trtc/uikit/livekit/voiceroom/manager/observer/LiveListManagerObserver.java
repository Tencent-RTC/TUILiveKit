package com.trtc.uikit.livekit.voiceroom.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("LiveListManagerObserver");

    protected VoiceRoomManager mVoiceRoomManager;

    public LiveListManagerObserver(VoiceRoomManager liveController) {
        mVoiceRoomManager = liveController;
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        LOGGER.info(hashCode() + " onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        mVoiceRoomManager.getRoomManager().onLiveInfoChanged(liveInfo, modifyFlagList);
    }
}
