package com.trtc.uikit.livekit.liveroom.core;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.util.HashMap;
import java.util.Map;

public class EngineManager {
    private static EngineManager sInstance;

    public RoomEngineService              mAnchorEngineService;
    public Map<String, RoomEngineService> mRoomEngineMap = new HashMap<>();

    public static EngineManager sharedInstance() {
        if (sInstance == null) {
            synchronized (EngineManager.class) {
                if (sInstance == null) {
                    sInstance = new EngineManager();
                }
            }
        }
        return sInstance;
    }

    public static void destroyInstance() {
        if (sInstance != null) {
            sInstance.destroy();
        }
        sInstance = null;
    }

    private EngineManager() {
    }

    private void destroy() {
    }

    public static void login(int sdkAppId, String userId, String userSig) {
        LiveKitLog.info("EngineManager login:[userId:" + userId);
        TUIRoomEngine.login(TUILogin.getAppContext(), sdkAppId, userId, userSig,
                new TUIRoomDefine.ActionCallback() {
                    @Override
                    public void onSuccess() {
                        LiveKitLog.info("EngineManager login:[Success]");
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LiveKitLog.error("EngineManager login:[Error:" + error + ",message:" + message + "]");
                    }
                });
    }
}
