package com.trtc.uikit.livekit.livestream;

import android.content.Context;

import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;

public interface VideoLiveKit {

    static VideoLiveKit createInstance(Context context) {
        return VideoLiveKitImpl.createInstance(context);
    }

    void startLive(String roomId);

    void joinLive(String roomId);
}
