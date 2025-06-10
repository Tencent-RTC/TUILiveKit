package com.trtc.uikit.livekit.livestream.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.DASHBOARD;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.PREVIEWING;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus.PRIVACY;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus.PUBLIC;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.RoomState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("RoomManager");

    public RoomManager(LiveState state, ILiveService service) {
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
        mRoomState.ownerInfo.userId = mUserState.selfInfo.userId;
        mRoomState.ownerInfo.name.setValue(mUserState.selfInfo.name.getValue());
        mRoomState.ownerInfo.avatarUrl.setValue(mUserState.selfInfo.avatarUrl.getValue());
        mUserState.selfInfo.role.setValue(TUIRoomDefine.Role.ROOM_OWNER);
    }

    public void startPreview() {
        mRoomState.liveStatus.setValue(PREVIEWING);
    }

    public void setRoomName(String roomName) {
        mRoomState.roomName.setValue(roomName);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.setValue(url);
    }

    public String getDefaultRoomName() {
        if (TextUtils.isEmpty(mUserState.selfInfo.name.getValue())) {
            return mUserState.selfInfo.userId;
        } else {
            return mUserState.selfInfo.name.getValue();
        }
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
        if (roomInfo.ownerId != null) {
            mRoomState.ownerInfo.userId = roomInfo.ownerId;
        }
        if (roomInfo.ownerName != null) {
            mRoomState.ownerInfo.name.setValue(roomInfo.ownerName);
        }
        if (roomInfo.ownerAvatarUrl != null) {
            mRoomState.ownerInfo.avatarUrl.setValue(roomInfo.ownerAvatarUrl);
        }
        mRoomState.roomInfo = roomInfo;
    }

    public void updateLiveStatus(RoomState.LiveStatus liveStatus) {
        if (mRoomState.liveStatus.getValue() == liveStatus) {
            return;
        }
        mRoomState.liveStatus.setValue(liveStatus);
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
        liveInfo.isPublicVisible = PUBLIC == mRoomState.liveMode.getValue();
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        mLiveService.setLiveInfo(liveInfo, flagList, null);
    }

    public void getLiveInfo(String roomId) {
        mLiveService.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                updateLiveInfo(liveInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorLocalized.onError(error);
            }
        });
    }

    public void setLiveInfo(String roomName, String coverUrl, RoomState.LiveStreamPrivacyStatus privacyStatus) {
        mRoomState.roomName.setValue(roomName);
        mRoomState.coverURL.setValue(coverUrl);
        mRoomState.liveMode.setValue(privacyStatus);
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
                mRoomState.liveMode.setValue(liveInfo.isPublicVisible ? PUBLIC : PRIVACY);
                break;
            default:
                break;
        }
    }

    public void onLiveEnd(String roomId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_room_destroy));
        updateLiveStatus(DASHBOARD);
    }

    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason) {
        if (mUserState.selfInfo.role.getValue() == TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(R.string.common_kicked_out_of_room_by_owner));
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", roomId);
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        }
    }

    public void startStreaming(String roomName, String coverUrl, RoomState.LiveStreamPrivacyStatus privacyStatus) {
        mRoomState.roomName.setValue(roomName);
        mRoomState.coverURL.setValue(coverUrl);
        mRoomState.liveMode.setValue(privacyStatus);
        mRoomState.liveStatus.setValue(RoomState.LiveStatus.PUSHING);
    }
}
