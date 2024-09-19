package com.trtc.uikit.livekit;

import android.content.Context;

import com.trtc.uikit.livekit.view.liveroom.VideoLiveKitImpl;

public interface VideoLiveKit {

    static VideoLiveKit createInstance(Context context) {
        return VideoLiveKitImpl.createInstance(context);
    }

    void startLive(String roomId);

    void joinLive(String roomId);
}
