package com.trtc.uikit.livekit.livestream.state;

import com.trtc.tuikit.common.livedata.LiveData;

public class BeautyState {
    public LiveData<Integer> smoothLevel         = new LiveData<>(6);
    public LiveData<Integer> whitenessLevel      = new LiveData<>(6);
    public LiveData<Integer> ruddyLevel          = new LiveData<>(6);
    public LiveData<Boolean> glContextCreateFlag = new LiveData<>();


    public void reset() {
        smoothLevel.set(6);
        whitenessLevel.set(6);
        ruddyLevel.set(6);
        glContextCreateFlag.set(null);
    }
}
