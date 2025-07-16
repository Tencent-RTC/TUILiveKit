package com.trtc.uikit.livekit.component.barrage.store;

import androidx.lifecycle.MutableLiveData;

import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;

import java.util.ArrayList;
import java.util.List;

public class BarrageState {
    public       String                         mRoomId            = "";
    public       String                         mOwnerId           = "";
    public final MutableLiveData<Integer>       mBarrageTotalCount = new MutableLiveData<>(0);
    public final MutableLiveData<List<Barrage>> mBarrageCacheList  = new MutableLiveData<>(new ArrayList<>());
}
