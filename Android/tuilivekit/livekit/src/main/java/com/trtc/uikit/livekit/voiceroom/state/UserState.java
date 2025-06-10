package com.trtc.uikit.livekit.voiceroom.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;

import java.util.HashMap;
import java.util.Map;

public class UserState {
    public UserInfo                               selfInfo            = new UserInfo();
    public MutableLiveData<Map<String, UserInfo>> userList            = new MutableLiveData<>(new HashMap<>());
    public MutableLiveData<Map<String, UserInfo>> myFollowingUserList = new MutableLiveData<>(new HashMap<>());
    public MutableLiveData<UserInfo>              enterUserInfo       = new MutableLiveData<>();

    public void reset() {
        userList.getValue().clear();
        myFollowingUserList.getValue().clear();
    }
}
