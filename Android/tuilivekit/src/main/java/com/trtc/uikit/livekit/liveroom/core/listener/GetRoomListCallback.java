package com.trtc.uikit.livekit.liveroom.core.listener;

import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

import java.util.List;

public interface GetRoomListCallback {
    void onSuccess(List<LiveRoomInfo> roomList);

    void onError(int errorCode, String errorMessage);
}
