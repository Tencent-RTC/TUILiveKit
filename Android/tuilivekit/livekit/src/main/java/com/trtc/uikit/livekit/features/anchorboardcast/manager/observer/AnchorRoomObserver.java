package com.trtc.uikit.livekit.features.anchorboardcast.manager.observer;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

public class AnchorRoomObserver extends TUIRoomObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorRoomObserver");

    protected AnchorManager mManager;

    public AnchorRoomObserver(AnchorManager manager) {
        mManager = manager;
    }


    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mManager.onRemoteUserEnterRoom(roomId, userInfo);
    }
}
