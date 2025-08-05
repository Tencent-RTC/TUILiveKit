package com.trtc.uikit.livekit.ktv;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.ktv.impl.KTVRoomKitImpl;

public interface KTVRoomKit {

    static KTVRoomKit createInstance(Context context) {
        return  KTVRoomKitImpl.createInstance(context);
    }

    void startLive(String roomId);

    void stopLive(TUIRoomDefine.ActionCallback callback);

    void joinLive(String roomId);

    void joinLive(LiveInfo liveInfo);

    void leaveLive(TUIRoomDefine.ActionCallback callback);
}
