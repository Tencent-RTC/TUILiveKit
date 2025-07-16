package com.trtc.uikit.livekit.component.beauty.basicbeauty.service;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.beauty.TXBeautyManager;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState;

public class BasicBeautyService {
    private static final LiveKitLogger    LOGGER = LiveKitLogger.getComponentLogger("BasicBeautyService");
    private final        BasicBeautyState mState = new BasicBeautyState();

    public BasicBeautyService() {
        TUIRoomEngine.sharedInstance().getTRTCCloud().getBeautyManager().setBeautyStyle(TXBeautyManager.TXBeautyStyleSmooth);
    }

    public BasicBeautyState getState() {
        return mState;
    }

    public void resetSettings() {
        closeBeauty();
        mState.roomId = "";
    }

    public void setBeautyLevel(int level) {
        LOGGER.info(hashCode() + " setBeautyLevel:[level:" + level + "]");
        TUIRoomEngine.sharedInstance().getTRTCCloud().getBeautyManager().setBeautyLevel(level);
        mState.smoothLevel.setValue(level);
    }

    public void setWhitenessLevel(int level) {
        LOGGER.info(hashCode() + " setWhitenessLevel:[level:" + level + "]");
        TUIRoomEngine.sharedInstance().getTRTCCloud().getBeautyManager().setWhitenessLevel(level);
        mState.whitenessLevel.setValue(level);
    }

    public void setRuddyLevel(int level) {
        LOGGER.info(hashCode() + " setRuddyLevel:[level:" + level + "]");
        TUIRoomEngine.sharedInstance().getTRTCCloud().getBeautyManager().setRuddyLevel(level);
        mState.ruddyLevel.setValue(level);
    }

    public void closeBeauty() {
        LOGGER.info(hashCode() + " closeBeauty:[]");
        setBeautyLevel(0);
        setWhitenessLevel(0);
        setRuddyLevel(0);
    }
}
