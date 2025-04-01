package com.trtc.uikit.livekit.livestream.state;

import androidx.lifecycle.MutableLiveData;

public class BeautyState {
    public MutableLiveData<Integer> smoothLevel    = new MutableLiveData<>(0);
    public MutableLiveData<Integer> whitenessLevel = new MutableLiveData<>(0);
    public MutableLiveData<Integer> ruddyLevel     = new MutableLiveData<>(0);

    public void reset() {
        smoothLevel.setValue(0);
        whitenessLevel.setValue(0);
        ruddyLevel.setValue(0);
    }
}
