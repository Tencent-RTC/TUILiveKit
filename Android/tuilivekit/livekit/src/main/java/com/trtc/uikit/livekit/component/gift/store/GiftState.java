package com.trtc.uikit.livekit.component.gift.store;

import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;

public class GiftState {
    public       String                              mRoomId        = "";
    public final MutableLiveData<List<GiftSendData>> mGiftCacheList = new MutableLiveData<>(new ArrayList<>());
}
