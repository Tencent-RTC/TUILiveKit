package com.trtc.uikit.livekit.common.uicomponent.roomlist.store;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class RoomListState {

    public final LiveData<List<LiveInfo>> mLiveList      = new LiveData<>(new ArrayList<>());
    public final LiveData<Boolean>        mRefreshStatus = new LiveData<>(false);
    public final LiveData<Boolean>        mLoadStatus    = new LiveData<>(false);

    public String  mFetchListCursor = "";
}
