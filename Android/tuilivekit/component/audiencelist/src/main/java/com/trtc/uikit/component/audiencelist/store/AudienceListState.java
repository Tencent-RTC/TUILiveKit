package com.trtc.uikit.component.audiencelist.store;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;

public class AudienceListState {
    public String                                          roomId;
    public String                                          ownerId;
    public LiveData<Integer>                               audienceCount = new LiveData<>(0);
    public LiveData<LinkedHashSet<TUIRoomDefine.UserInfo>> audienceList  = new LiveData<>(new LinkedHashSet<>());
}
