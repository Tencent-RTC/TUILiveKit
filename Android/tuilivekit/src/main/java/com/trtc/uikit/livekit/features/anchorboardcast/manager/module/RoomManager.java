package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.Constants;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;

import java.util.List;

public class RoomManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("RoomManager");

    public RoomManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public void initCreateRoomState(LiveInfo liveInfo) {
        LOGGER.info("initCreateRoomState roomId [roomId: " + liveInfo.roomId + ", roomName:" + liveInfo.name);
        mRoomState.roomId = liveInfo.roomId;
        liveInfo.createTime = System.currentTimeMillis();
        if (TextUtils.isEmpty(liveInfo.coverUrl)) {
            liveInfo.coverUrl = Constants.DEFAULT_COVER_URL;
        }
        if (TextUtils.isEmpty(liveInfo.backgroundUrl)) {
            liveInfo.backgroundUrl = Constants.DEFAULT_BACKGROUND_URL;
        }
    }

    public void updateRoomState(LiveInfo liveInfo) {
        if (liveInfo.createTime == 0) {
            liveInfo.createTime = System.currentTimeMillis();
        }
        mRoomState.liveInfo = liveInfo;
    }

    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mLiveService.getLiveInfo(roomId, callback);
    }

    public void onLiveInfoChanged(LiveInfo liveInfo, List<LiveModifyFlag> modifyFlagList) {
        for (LiveModifyFlag flag : modifyFlagList) {
            onLiveInfoChanged(flag, liveInfo);
        }
    }

    private void onLiveInfoChanged(LiveModifyFlag flag, LiveInfo liveInfo) {
        switch (flag) {
            case COVER_URL:
                mRoomState.liveInfo.coverUrl = liveInfo.coverUrl;
                break;
            case BACKGROUND_URL:
                mRoomState.liveInfo.backgroundUrl = liveInfo.backgroundUrl;
                break;
            case ACTIVITY_STATUS:
                mRoomState.liveInfo.activityStatus = liveInfo.activityStatus;
                break;
            case PUBLISH:
                mRoomState.liveInfo.isPublicVisible = liveInfo.isPublicVisible;
                break;
            default:
                break;
        }
    }
}
