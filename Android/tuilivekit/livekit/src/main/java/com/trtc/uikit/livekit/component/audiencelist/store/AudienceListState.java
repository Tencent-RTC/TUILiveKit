package com.trtc.uikit.livekit.component.audiencelist.store;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.LinkedHashSet;
import java.util.Set;

public class AudienceListState {
    public static final int ROOM_MAX_SHOW_USER_COUNT        = 100;
    public static final int FETCH_LIST_DURATION_MILLISECOND = 10000;

    public String                                       roomId;
    public String                                       ownerId;
    public MutableLiveData<Boolean>                     isRoomDismissed = new MutableLiveData<>(false);
    public MutableLiveData<Integer>                     audienceCount   = new MutableLiveData<>(0);
    public MutableLiveData<Set<TUIRoomDefine.UserInfo>> audienceList    = new MutableLiveData<>(new LinkedHashSet<>());
    public long                                         lastFetchTime   = 0;
}
