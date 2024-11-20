package com.trtc.uikit.livekit.voiceroom.manager.module;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
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

    public void updateMicrophoneOpenState(boolean isOpened) {
        mMediaState.isMicrophoneOpened.set(isOpened);
    }

    public void updateMicrophoneMuteState(boolean isMuted) {
        mMediaState.isMicrophoneMuted.set(isMuted);
    }

    public void onUserAudioStateChanged(TUIRoomDefine.UserInfo userInfo, boolean hasAudio,
                                        TUIRoomDefine.ChangeReason reason) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            updateMicrophoneMuteState(!hasAudio);
            if (hasAudio) {
                updateMicrophoneOpenState(true);
            }
        }
    }
}
