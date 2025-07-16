package com.trtc.uikit.livekit.features.anchorboardcast.state;

import androidx.lifecycle.MutableLiveData;

public class AnchorConfig {
    public static MutableLiveData<Boolean> disableHeaderLiveData        = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableHeaderVisitorCnt      = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableFooterCoGuest         = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableFooterCoHost          = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableFooterBattle          = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableFooterSoundEffect     = new MutableLiveData<>(false);
}
