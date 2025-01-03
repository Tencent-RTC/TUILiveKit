package com.trtc.uikit.component.gift.store;

import com.trtc.tuikit.common.livedata.LiveData;

public class LikeState {
    public final LiveData<Integer> mLikeReceivedTotalCount = new LiveData<>(0);
    public final LiveData<Boolean> mLikeAnimationTrigger   = new LiveData<>(false);
}
