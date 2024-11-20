package com.trtc.uikit.livekit.livestreamcore.state;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;

public class UserState {
    public UserInfo                        selfInfo               = new UserInfo();
    public LiveData<LinkedHashSet<String>> hasAudioStreamUserList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>> hasVideoStreamUserList = new LiveData<>(new LinkedHashSet<>());

    public void reset() {
        hasAudioStreamUserList.get().clear();
        hasVideoStreamUserList.get().clear();
    }
}
