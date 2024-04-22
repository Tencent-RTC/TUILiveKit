package com.trtc.uikit.livekit.common.core.store.state.view;

import com.trtc.tuikit.common.livedata.LiveData;

public class ViewState {
    public LiveData<NavigationState> currentNavigationState = new LiveData<>(NavigationState.MAIN);
    public LiveData<Boolean>         showAnchorPreview      = new LiveData<>(false);
    public LiveData<Boolean>         showEndView            = new LiveData<>(false);

    public enum NavigationState {
        MAIN,
        EXIT,
        LINK_MIC,
        LINK_MANAGEMENT,
        MUSIC,
        GIFT,
        LIKE
    }
}
