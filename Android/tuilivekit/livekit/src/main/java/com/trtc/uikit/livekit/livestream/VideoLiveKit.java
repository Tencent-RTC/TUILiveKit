package com.trtc.uikit.livekit.livestream;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.livestream.view.VideoLiveKitImpl;

public interface VideoLiveKit {

    static VideoLiveKit createInstance(Context context) {
        return VideoLiveKitImpl.createInstance(context);
    }

    void startLive(String roomId);

    void stopLive(TUIRoomDefine.ActionCallback callback);

    void joinLive(String roomId);

    void leaveLive(TUIRoomDefine.ActionCallback callback);

    void enableFollowFeature(boolean enable);
}
