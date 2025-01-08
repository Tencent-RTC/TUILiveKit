package com.trtc.uikit.component.roominfo.store;

import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;

public class RoomInfoState {
    public String                          myUserId       = "";
    public String                          roomId         = "";
    public boolean                         enableFollow   = true;
    public LiveData<String>                ownerId        = new LiveData<>("");
    public LiveData<String>                ownerName      = new LiveData<>("");
    public LiveData<String>                ownerAvatarUrl = new LiveData<>("");
    public LiveData<Long>                  fansNumber     = new LiveData<>(0L);
    public LiveData<LinkedHashSet<String>> followingList  = new LiveData<>(new LinkedHashSet<>());
}
