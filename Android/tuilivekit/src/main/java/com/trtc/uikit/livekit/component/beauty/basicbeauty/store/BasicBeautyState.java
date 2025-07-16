package com.trtc.uikit.livekit.component.beauty.basicbeauty.store;

import androidx.lifecycle.MutableLiveData;

public class BasicBeautyState {
    public static final int ITEM_BEAUTY_CLOSE     = 201;
    public static final int ITEM_BEAUTY_SMOOTH    = 202;
    public static final int ITEM_BEAUTY_WHITENESS = 203;
    public static final int ITEM_BEAUTY_RUDDY     = 204;

    public MutableLiveData<Integer> smoothLevel        = new MutableLiveData<>(0);
    public MutableLiveData<Integer> whitenessLevel     = new MutableLiveData<>(0);
    public MutableLiveData<Integer> ruddyLevel         = new MutableLiveData<>(0);
    public MutableLiveData<Integer> mCurrentBeautyType = new MutableLiveData<>(ITEM_BEAUTY_CLOSE);
    public String                   roomId             = "";
}
