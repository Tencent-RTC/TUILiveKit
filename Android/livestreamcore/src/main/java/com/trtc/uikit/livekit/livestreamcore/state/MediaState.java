package com.trtc.uikit.livekit.livestreamcore.state;

import com.trtc.tuikit.common.livedata.LiveData;

public class MediaState {
    public LiveData<Boolean> hasMicrophonePermission = new LiveData<>(false);
    public LiveData<Boolean> isMicrophoneOpened      = new LiveData<>(false);
    public LiveData<Boolean> isMicrophoneMuted       = new LiveData<>(true);
    public LiveData<Boolean> hasCameraPermission     = new LiveData<>(false);
    public LiveData<Boolean> isCameraOpened          = new LiveData<>(false);
    public LiveData<Boolean> isFrontCamera           = new LiveData<>(true);

    public void reset() {
        hasMicrophonePermission.set(false);
        isMicrophoneOpened.set(false);
        isMicrophoneMuted.set(true);
        hasCameraPermission.set(false);
        isCameraOpened.set(false);
        isFrontCamera.set(true);
    }
}
