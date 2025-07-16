package com.trtc.uikit.livekit.features.audiencecontainer.state;

import androidx.lifecycle.MutableLiveData;

public class AudienceContainerConfig {
    public static MutableLiveData<Boolean> disableSliding          = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableHeaderFloatWin   = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableHeaderLiveData   = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableHeaderVisitorCnt = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableFooterCoGuest    = new MutableLiveData<>(false);
}

