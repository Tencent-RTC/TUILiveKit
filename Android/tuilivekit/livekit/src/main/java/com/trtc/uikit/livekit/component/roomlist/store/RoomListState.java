package com.trtc.uikit.livekit.component.roomlist.store;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;

import java.util.ArrayList;
import java.util.List;

public class RoomListState {

    public final MutableLiveData<List<LiveInfo>> mLiveList        = new MutableLiveData<>(new ArrayList<>());
    public final MutableLiveData<Boolean>        mRefreshStatus   = new MutableLiveData<>(false);
    public final MutableLiveData<Boolean>        mLoadStatus      = new MutableLiveData<>(false);
    public       String                          mFetchListCursor = "";
}
