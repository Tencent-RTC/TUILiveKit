package com.trtc.uikit.livekit.component.roominfo.store;

import androidx.lifecycle.MutableLiveData;

import java.util.LinkedHashSet;
import java.util.Set;

public class RoomInfoState {
    public String                       myUserId       = "";
    public String                       roomId         = "";
    public boolean                      enableFollow   = true;
    public MutableLiveData<String>      ownerId        = new MutableLiveData<>("");
    public MutableLiveData<String>      ownerName      = new MutableLiveData<>("");
    public MutableLiveData<String>      ownerAvatarUrl = new MutableLiveData<>("");
    public MutableLiveData<Long>        fansNumber     = new MutableLiveData<>(0L);
    public MutableLiveData<Set<String>> followingList  = new MutableLiveData<>(new LinkedHashSet<>());
}
