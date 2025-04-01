package com.trtc.uikit.livekit.component.gift.store;

import androidx.lifecycle.MutableLiveData;

public class LikeState {
    public       String                   mRoomId                 = "";
    public final MutableLiveData<Integer> mLikeReceivedTotalCount = new MutableLiveData<>(0);
    public final MutableLiveData<Boolean> mLikeAnimationTrigger   = new MutableLiveData<>(false);
}
