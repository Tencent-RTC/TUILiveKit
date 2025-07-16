package com.trtc.uikit.livekit.livestream;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.livestream.impl.VideoLiveKitImpl;

public interface VideoLiveKit {

    static VideoLiveKit createInstance(Context context) {
        return VideoLiveKitImpl.createInstance(context);
    }

    void startLive(String roomId);

    void stopLive(TUIRoomDefine.ActionCallback callback);

    void joinLive(String roomId);

    void joinLive(LiveInfo liveInfo);

    void leaveLive(TUIRoomDefine.ActionCallback callback);
}
