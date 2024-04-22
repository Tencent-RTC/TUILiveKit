package com.trtc.uikit.livekit.liveroom.core.listener;

import com.trtc.uikit.livekit.liveroom.data.UserInfo;

public interface GetUserInfoCallback {
    void onSuccess(UserInfo userInfo);

    void onError(int errorCode, String errorMessage);
}
