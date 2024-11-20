package com.trtc.uikit.livekit.component.floatwindow.store;

import com.trtc.tuikit.common.livedata.LiveData;

public final class FloatWindowStore {

    public boolean isWillOpenFloatWindow = false;

    public final LiveData<Boolean> isShowingFloatWindow = new LiveData<>();

    public FloatWindowStore() {
    }

}
