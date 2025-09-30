package com.trtc.uikit.livekit.voiceroom.state;

import androidx.lifecycle.MutableLiveData;

public class MediaState {
    public MutableLiveData<Boolean> hasMicrophonePermission = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isMicrophoneOpened = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isMicrophoneMuted = new MutableLiveData<>(true);

    public void reset() {
        isMicrophoneMuted.setValue(true);
    }
}
