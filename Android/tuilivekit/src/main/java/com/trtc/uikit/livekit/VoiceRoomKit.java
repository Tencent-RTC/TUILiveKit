package com.trtc.uikit.livekit;

import android.content.Context;
import com.trtc.uikit.livekit.view.voiceroom.VoiceRoomKitImpl;

public interface VoiceRoomKit {

    static VoiceRoomKit createInstance(Context context) {
        return VoiceRoomKitImpl.createInstance(context);
    }

    void createRoom(String roomId, VoiceRoomDefine.CreateRoomParams info);

    void enterRoom(String roomId);
}
