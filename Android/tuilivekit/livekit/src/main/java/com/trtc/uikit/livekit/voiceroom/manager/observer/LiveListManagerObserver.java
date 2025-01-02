package com.trtc.uikit.livekit.voiceroom.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.List;

public class LiveListManagerObserver extends TUILiveListManager.Observer {
    private final String FILE = "LiveListManagerObserver[" + hashCode() + "]";

    protected VoiceRoomManager mVoiceRoomManager;

    public LiveListManagerObserver(VoiceRoomManager liveController) {
        mVoiceRoomManager = liveController;
    }

    @Override
    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        Logger.info(FILE, "onLiveInfoChanged:[liveInfo:" + new Gson().toJson(liveInfo) + ",modifyFlagList:");
        mVoiceRoomManager.getRoomManager().onLiveInfoChanged(liveInfo, modifyFlagList);
    }
}
