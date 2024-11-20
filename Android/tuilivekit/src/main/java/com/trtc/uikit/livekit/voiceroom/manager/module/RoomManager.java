package com.trtc.uikit.livekit.voiceroom.manager.module;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

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
import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomManager extends BaseManager {
    private static final String FILE = "RoomManager";

    private final List<String> mCategoryList = new ArrayList<>();

    public RoomManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        Logger.info(FILE, " destroy");
    }

    public void initCreateRoomState(String roomId, String roomName, TUIRoomDefine.SeatMode seatMode, int maxSeatCount) {
        Logger.info(FILE, " initCreateRoomState roomId [roomId: " + roomId + ", roomName:" + roomName + ", "
                + "seatMode:" + seatMode + ", maxSeatCount:" + maxSeatCount + "]");
        mRoomState.roomId = roomId;
        mRoomState.roomName.set(roomName);
        mRoomState.seatMode.set(seatMode);
        mRoomState.maxSeatCount.set(maxSeatCount);
        mRoomState.createTime = System.currentTimeMillis();
        mRoomState.ownerInfo.userId = mUserState.selfInfo.userId;
        mRoomState.ownerInfo.name.set(mUserState.selfInfo.name.get());
        mRoomState.ownerInfo.avatarUrl.set(mUserState.selfInfo.avatarUrl.get());
        mUserState.selfInfo.role.set(TUIRoomDefine.Role.ROOM_OWNER);
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.updateState(roomInfo);
    }

    public void updateLiveInfo() {
        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.coverUrl = mRoomState.coverURL.get();
        liveInfo.backgroundUrl = mRoomState.backgroundURL.get();
        liveInfo.isPublicVisible = RoomState.LiveStreamPrivacyStatus.PUBLIC == mRoomState.liveExtraInfo.liveMode.get();
        String category = mRoomState.liveExtraInfo.category.get();
        for (int i = 0; i < mCategoryList.size(); i++) {
            String item = mCategoryList.get(i);
            if (TextUtils.equals(item, category) && RoomState.LiveCategory.getCategory(i) != null) {
                liveInfo.categoryList = Collections.singletonList(i);
                break;
            }
        }
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.CATEGORY);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        mLiveService.setLiveInfo(liveInfo, flagList, null);
    }

    public void updateLiveStatus(RoomState.LiveStatus status) {
        mRoomState.liveStatus.set(status);
    }

    public void getLiveInfo(String roomId) {
        mLiveService.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                mRoomState.coverURL.set(liveInfo.coverUrl, false);
                mRoomState.backgroundURL.set(liveInfo.backgroundUrl, false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorLocalized.onError(error);
            }
        });
    }

    public void clearLiveState() {
        mRoomState.liveStatus.set(RoomState.LiveStatus.NONE);
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
        mRoomState.roomName.set(roomName, false);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.set(url, false);
    }

    public void setBackgroundURL(String backgroundURL) {
        if (TextUtils.isEmpty(backgroundURL)) {
            return;
        }
        if (backgroundURL.equals(mRoomState.backgroundURL.get())) {
            return;
        }
        if (mRoomState.liveStatus.get() == RoomState.LiveStatus.PREVIEWING) {
            mRoomState.backgroundURL.set(backgroundURL, false);
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
                mRoomState.backgroundURL.set(backgroundURL, false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorLocalized.onError(error);
            }
        });
    }

    public String getDefaultRoomName() {
        if (TextUtils.isEmpty(mUserState.selfInfo.name.get())) {
            return mUserState.selfInfo.userId;
        } else {
            return mUserState.selfInfo.name.get();
        }
    }

    public void setLiveCategoryList(List<String> list) {
        if (mCategoryList.isEmpty()) {
            mCategoryList.addAll(list);
        }
    }

    public List<String> getLiveCategoryList() {
        return mCategoryList;
    }

    public void setLiveCategory(String category) {
        mRoomState.liveExtraInfo.category.set(category);
    }

    public void updateSeatMode(TUIRoomDefine.SeatMode seatMode) {
        mRoomState.seatMode.set(seatMode, false);
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
            mRoomState.userCount.set(userCount - 1);
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
                mRoomState.coverURL.set(liveInfo.coverUrl, false);
                break;
            case BACKGROUND_URL:
                mRoomState.backgroundURL.set(liveInfo.backgroundUrl, false);
                break;
            default:
                break;
        }
    }

    /******************************************  Observer *******************************************/
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        Logger.info(FILE, "onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ErrorLocalized.handleMessage(message);
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", roomId);
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        }
    }

    public void onRoomDismissed(String roomId) {
        ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(R.string.livekit_room_destroy));
        updateLiveStatus(RoomState.LiveStatus.DASHBOARD);
    }
}
