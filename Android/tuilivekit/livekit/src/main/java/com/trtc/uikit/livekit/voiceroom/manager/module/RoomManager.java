package com.trtc.uikit.livekit.voiceroom.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("RoomManager");

    public RoomManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
    }

    public void initCreateRoomState(String roomId, String roomName, TUIRoomDefine.SeatMode seatMode, int maxSeatCount) {
        LOGGER.info("initCreateRoomState roomId [roomId: " + roomId + ", roomName:" + roomName + ", "
                + "seatMode:" + seatMode + ", maxSeatCount:" + maxSeatCount + "]");
        mRoomState.roomId = roomId;
        mRoomState.roomName.setValue(roomName);
        mRoomState.seatMode.setValue(seatMode);
        mRoomState.maxSeatCount.setValue(maxSeatCount);
        mRoomState.createTime = System.currentTimeMillis();
        mRoomState.ownerInfo.userId = mUserState.selfInfo.userId;
        mRoomState.ownerInfo.userName = mUserState.selfInfo.userName;
        mRoomState.ownerInfo.avatarUrl = mUserState.selfInfo.avatarUrl;
        mUserState.selfInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.updateState(roomInfo);
    }

    public void updateLiveInfo() {
        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.coverUrl = mRoomState.coverURL.getValue();
        liveInfo.backgroundUrl = mRoomState.backgroundURL.getValue();
        liveInfo.isPublicVisible =
                RoomState.LiveStreamPrivacyStatus.PUBLIC == mRoomState.liveExtraInfo.liveMode.getValue();
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        mLiveService.setLiveInfo(liveInfo, flagList, null);
    }

    public void updateLiveStatus(RoomState.LiveStatus status) {
        mRoomState.liveStatus.setValue(status);
    }

    public void getLiveInfo(String roomId) {
        mLiveService.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                mRoomState.coverURL.setValue(liveInfo.coverUrl);
                mRoomState.backgroundURL.setValue(liveInfo.backgroundUrl);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorLocalized.onError(error);
            }
        });
    }

    public void clearLiveState() {
        mRoomState.liveStatus.setValue(RoomState.LiveStatus.NONE);
    }

    public void updateMessageCount(int messageCount) {
        mRoomState.liveExtraInfo.messageCount = messageCount;
    }

    public void updateGiftIncome(int giftIncome) {
        mRoomState.liveExtraInfo.giftIncome = giftIncome;
    }

    public void insertGiftPeople(String userId) {
        mRoomState.liveExtraInfo.giftPeopleSet.add(userId);
    }

    public void updateLikeNumber(int messageCount) {
        mRoomState.liveExtraInfo.likeCount = messageCount;
    }

    public void setRoomName(String roomName) {
        mRoomState.roomName.setValue(roomName);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.setValue(url);
    }

    public void setBackgroundURL(String backgroundURL) {
        if (TextUtils.isEmpty(backgroundURL)) {
            return;
        }
        if (backgroundURL.equals(mRoomState.backgroundURL.getValue())) {
            return;
        }
        if (mRoomState.liveStatus.getValue() == RoomState.LiveStatus.PREVIEWING) {
            mRoomState.backgroundURL.setValue(backgroundURL);
            return;
        }

        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.backgroundUrl = backgroundURL;
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        mLiveService.setLiveInfo(liveInfo, flagList, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mRoomState.backgroundURL.setValue(backgroundURL);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("setLiveInfo failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public String getDefaultRoomName() {
        if (TextUtils.isEmpty(mUserState.selfInfo.userName)) {
            return mUserState.selfInfo.userId;
        } else {
            return mUserState.selfInfo.userName;
        }
    }

    public void updateSeatMode(TUIRoomDefine.SeatMode seatMode) {
        mRoomState.seatMode.setValue(seatMode);
    }

    public boolean isOwner() {
        String selfUserId = TUIRoomEngine.getSelfInfo().userId;
        if (TextUtils.isEmpty(selfUserId)) {
            return false;
        }
        return selfUserId.equals(mRoomState.ownerInfo.userId);
    }

    public void onRoomUserCountChanged(String roomId, int userCount) {
        if (userCount > 0) {
            mRoomState.userCount.setValue(userCount - 1);
            if (userCount > mRoomState.liveExtraInfo.maxAudienceCount) {
                mRoomState.liveExtraInfo.maxAudienceCount = userCount - 1;
            }
        }
    }

    public void onLiveInfoChanged(TUILiveListManager.LiveInfo liveInfo,
                                  List<TUILiveListManager.LiveModifyFlag> modifyFlagList) {
        for (TUILiveListManager.LiveModifyFlag flag : modifyFlagList) {
            onLiveInfoChanged(flag, liveInfo);
        }
    }

    private void onLiveInfoChanged(TUILiveListManager.LiveModifyFlag flag, TUILiveListManager.LiveInfo liveInfo) {
        switch (flag) {
            case COVER_URL:
                mRoomState.coverURL.setValue(liveInfo.coverUrl);
                break;
            case BACKGROUND_URL:
                mRoomState.backgroundURL.setValue(liveInfo.backgroundUrl);
                break;
            default:
                break;
        }
    }

    /******************************************  Observer *******************************************/
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LOGGER.info("onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(R.string.common_kicked_out_of_room_by_owner));
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", roomId);
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        }
    }

    public void onRoomDismissed(String roomId) {
        ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(R.string.common_room_destroy));
        updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
    }
}
