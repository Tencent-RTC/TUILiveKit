package com.trtc.uikit.livekit.voiceroom.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class UserState {
    public UserInfo                               selfInfo          = new UserInfo();
    public MutableLiveData<Map<String, UserInfo>> userList          = new MutableLiveData<>(new HashMap<>());
    public MutableLiveData<Set<String>>           followingUserList = new MutableLiveData<>(new LinkedHashSet<>());

    public void reset() {
        userList.getValue().clear();
        followingUserList.getValue().clear();
    }
}
