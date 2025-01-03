package com.trtc.uikit.component.gift.store;

import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class GiftState {
    public final LiveData<List<GiftSendData>> mGiftCacheList = new LiveData<>(new ArrayList<>());
}
