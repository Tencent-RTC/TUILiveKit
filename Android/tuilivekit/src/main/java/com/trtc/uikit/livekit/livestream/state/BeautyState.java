package com.trtc.uikit.livekit.livestream.state;

import com.trtc.tuikit.common.livedata.LiveData;

public class BeautyState {
    public LiveData<Integer> smoothLevel         = new LiveData<>(0);
    public LiveData<Integer> whitenessLevel      = new LiveData<>(0);
    public LiveData<Integer> ruddyLevel          = new LiveData<>(0);
    public LiveData<Boolean> glContextCreateFlag = new LiveData<>();


    public void reset() {
        smoothLevel.set(0);
        whitenessLevel.set(0);
        ruddyLevel.set(0);
        glContextCreateFlag.set(null);
    }
}
