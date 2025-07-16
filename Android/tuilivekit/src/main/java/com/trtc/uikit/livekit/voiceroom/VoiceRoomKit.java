package com.trtc.uikit.livekit.voiceroom;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.uikit.livekit.voiceroom.view.VoiceRoomKitImpl;

public interface VoiceRoomKit {

    static VoiceRoomKit createInstance(Context context) {
        return VoiceRoomKitImpl.createInstance(context);
    }

    void createRoom(String roomId, VoiceRoomDefine.CreateRoomParams info);

    void enterRoom(String roomId);

    void enterRoom(TUILiveListManager.LiveInfo liveInfo);
}
