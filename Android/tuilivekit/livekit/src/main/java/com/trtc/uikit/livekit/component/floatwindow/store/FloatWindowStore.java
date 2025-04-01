package com.trtc.uikit.livekit.component.floatwindow.store;

import androidx.lifecycle.MutableLiveData;

public final class FloatWindowStore {

    public boolean isWillOpenFloatWindow = false;

    public final MutableLiveData<Boolean> isShowingFloatWindow = new MutableLiveData<>();

    public FloatWindowStore() {
    }

}
