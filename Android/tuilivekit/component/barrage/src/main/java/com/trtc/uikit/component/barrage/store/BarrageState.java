package com.trtc.uikit.component.barrage.store;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.component.barrage.store.model.Barrage;

import java.util.ArrayList;
import java.util.List;

public class BarrageState {
    public final LiveData<Integer>       mBarrageTotalCount = new LiveData<>(0);
    public final LiveData<List<Barrage>> mBarrageCacheList  = new LiveData<>(new ArrayList<>());
}
