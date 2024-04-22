package com.trtc.uikit.livekit.liveroom.core.listener;

import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.List;

public interface GetUserListCallback {
    void onSuccess(List<UserInfo> userList);

    void onError(int errorCode, String errorMessage);
}
