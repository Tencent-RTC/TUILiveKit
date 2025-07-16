package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import static java.lang.Boolean.TRUE;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;

import java.util.ArrayList;
import java.util.List;

public class RoomManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("RoomManager");

    public RoomManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public void initCreateRoomState(String roomId, String roomName) {
        LOGGER.info("initCreateRoomState roomId [roomId: " + roomId + ", roomName:" + roomName);
        mRoomState.roomId = roomId;
        mRoomState.roomName.setValue(roomName);
        mRoomState.createTime = System.currentTimeMillis();
    }

    public void setRoomName(String roomName) {
        mRoomState.roomName.setValue(roomName);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.setValue(url);
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        if (roomInfo.createTime != 0) {
            mRoomState.createTime = roomInfo.createTime;
        } else {
            mRoomState.createTime = System.currentTimeMillis();
        }
        if (roomInfo.name != null) {
            mRoomState.roomName.setValue(roomInfo.name);
        }
        mRoomState.roomInfo = roomInfo;
    }

    public void updateLiveInfo(LiveInfo liveInfo) {
        if (liveInfo == null) {
            return;
        }
        if (liveInfo.roomInfo != null) {
            updateRoomState(liveInfo.roomInfo);
        }
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.ACTIVITY_STATUS);
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        onLiveInfoChanged(liveInfo, flagList);
    }

    public void updateLiveInfo() {
        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.coverUrl = mRoomState.coverURL.getValue();
        liveInfo.backgroundUrl = mRoomState.backgroundURL.getValue();
        liveInfo.isPublicVisible = TRUE.equals(mRoomState.isPublicVisible.getValue());
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        mLiveService.setLiveInfo(liveInfo, flagList, null);
    }

    public void getLiveInfo(String roomId, TUILiveListManager.LiveInfoCallback callback) {
        mLiveService.getLiveInfo(roomId, callback);
    }

    public void setLiveInfo(String roomName, String coverUrl, boolean isPublicVisible) {
        mRoomState.roomName.setValue(roomName);
        mRoomState.coverURL.setValue(coverUrl);
        mRoomState.isPublicVisible.setValue(isPublicVisible);
    }

    public void onRoomUserCountChanged(String roomId, int userCount) {
        if (userCount > 0) {
            mRoomState.userCount.setValue(userCount - 1);
            if (userCount > mRoomState.maxAudienceCount) {
                mRoomState.maxAudienceCount = userCount - 1;
            }
        }
    }

    public void onLiveInfoChanged(LiveInfo liveInfo, List<LiveModifyFlag> modifyFlagList) {
        for (LiveModifyFlag flag : modifyFlagList) {
            onLiveInfoChanged(flag, liveInfo);
        }
    }

    private void onLiveInfoChanged(LiveModifyFlag flag, LiveInfo liveInfo) {
        switch (flag) {
            case COVER_URL:
                mRoomState.coverURL.setValue(liveInfo.coverUrl);
                break;
            case BACKGROUND_URL:
                mRoomState.backgroundURL.setValue(liveInfo.backgroundUrl);
                break;
            case ACTIVITY_STATUS:
                mRoomState.activityStatus.setValue(liveInfo.activityStatus);
                break;
            case PUBLISH:
                mRoomState.isPublicVisible.setValue(liveInfo.isPublicVisible);
                break;
            default:
                break;
        }
    }
}
