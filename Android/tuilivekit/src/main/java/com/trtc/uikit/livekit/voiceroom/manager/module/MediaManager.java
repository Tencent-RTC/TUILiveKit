package com.trtc.uikit.livekit.voiceroom.manager.module;

import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

public class MediaManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("MediaManager");

    public MediaManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
    }
}
