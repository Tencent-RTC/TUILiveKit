package com.trtc.uikit.component.audiencelist.store;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;

public class AudienceListState {
    public static final int ROOM_MAX_SHOW_USER_COUNT        = 100;
    public static final int FETCH_LIST_DURATION_MILLISECOND = 10000;

    public String                                          roomId;
    public String                                          ownerId;
    public LiveData<Boolean>                               isRoomDismissed = new LiveData<>(false);
    public LiveData<Integer>                               audienceCount   = new LiveData<>(0);
    public LiveData<LinkedHashSet<TUIRoomDefine.UserInfo>> audienceList    = new LiveData<>(new LinkedHashSet<>());
    public long                                            lastFetchTime   = 0;
}
