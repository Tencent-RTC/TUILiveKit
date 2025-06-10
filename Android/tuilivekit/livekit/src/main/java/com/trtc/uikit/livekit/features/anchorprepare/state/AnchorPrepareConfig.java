package com.trtc.uikit.livekit.features.anchorprepare.state;

import androidx.lifecycle.MutableLiveData;

public class AnchorPrepareConfig {
    public static MutableLiveData<Boolean> disableFeatureMenu           = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableMenuSwitchButton      = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableMenuBeautyButton      = new MutableLiveData<>(false);
    public static MutableLiveData<Boolean> disableMenuAudioEffectButton = new MutableLiveData<>(false);
}
