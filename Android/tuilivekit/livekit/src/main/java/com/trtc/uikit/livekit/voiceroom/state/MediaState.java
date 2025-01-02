package com.trtc.uikit.livekit.voiceroom.state;

import com.trtc.tuikit.common.livedata.LiveData;

public class MediaState {
    public LiveData<Boolean>                    isMicrophoneOpened      = new LiveData<>(false);
    public LiveData<Boolean>                    isMicrophoneMuted       = new LiveData<>(true);

    public void reset() {
        isMicrophoneOpened.set(false);
        isMicrophoneMuted.set(true);
    }
}
