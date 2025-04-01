package com.trtc.uikit.livekit.voiceroom.manager.module;

import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.manager.api.Logger;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

public class MediaManager extends BaseManager {
    private static final String FILE = "MediaManager";

    public MediaManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        Logger.info(FILE, " destroy");
    }
}
