package com.trtc.uikit.livekit.livestream.manager.module;

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
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.RoomState;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RoomManager extends BaseManager {
    private static final String       TAG           = "RoomManager";
    private final        List<String> mCategoryList = new ArrayList<>();

    public RoomManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public void initCreateRoomState(String roomId, String roomName, int maxSeatCount) {
        LiveStreamLog.info(TAG + " initCreateRoomState roomId [roomId: " + roomId + ", roomName:" + roomName + ", "
                + ", maxSeatCount:" + maxSeatCount + "]");
        mRoomState.roomId = roomId;
        mRoomState.roomName.set(roomName);
        mRoomState.maxSeatCount.set(maxSeatCount);
        mRoomState.createTime = System.currentTimeMillis();
        mRoomState.ownerInfo.userId = mUserState.selfInfo.userId;
        mRoomState.ownerInfo.name.set(mUserState.selfInfo.name.get());
        mRoomState.ownerInfo.avatarUrl.set(mUserState.selfInfo.avatarUrl.get());
        mUserState.selfInfo.role.set(TUIRoomDefine.Role.ROOM_OWNER);
    }

    public void startPreview() {
        mRoomState.liveStatus.set(PREVIEWING);
    }

    public void setRoomName(String roomName) {
        mRoomState.roomName.set(roomName, false);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.set(url, false);
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
        mRoomState.category.set(category);
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.createTime = roomInfo.createTime;
        if (roomInfo.name != null) {
            mRoomState.roomName.set(roomInfo.name, false);
        }
        if (roomInfo.ownerId != null) {
            mRoomState.ownerInfo.userId = roomInfo.ownerId;
        }
        if (roomInfo.ownerName != null) {
            mRoomState.ownerInfo.name.set(roomInfo.ownerName);
        }
        if (roomInfo.ownerAvatarUrl != null) {
            mRoomState.ownerInfo.avatarUrl.set(roomInfo.ownerAvatarUrl);
        }
        mRoomState.maxSeatCount.set(roomInfo.maxSeatCount, false);
    }

    public void updateLiveStatus(RoomState.LiveStatus liveStatus) {
        if (mRoomState.liveStatus.get() == liveStatus) {
            return;
        }
        mRoomState.liveStatus.set(liveStatus);
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
        flagList.add(LiveModifyFlag.CATEGORY);
        flagList.add(LiveModifyFlag.BACKGROUND_URL);
        onLiveInfoChanged(liveInfo, flagList);
    }

    public void updateLiveInfo() {
        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.coverUrl = mRoomState.coverURL.get();
        liveInfo.backgroundUrl = mRoomState.backgroundURL.get();
        liveInfo.isPublicVisible = PUBLIC == mRoomState.liveMode.get();
        String category = mRoomState.category.get();
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

    public void getLiveInfo(String roomId) {
        mLiveService.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                updateLiveInfo(liveInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void onRoomUserCountChanged(String roomId, int userCount) {
        if (userCount > 0) {
            mRoomState.userCount.set(userCount - 1);
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
                mRoomState.coverURL.set(liveInfo.coverUrl, false);
                break;
            case BACKGROUND_URL:
                mRoomState.backgroundURL.set(liveInfo.backgroundUrl, false);
                break;
            case ACTIVITY_STATUS:
                mRoomState.activityStatus.set(liveInfo.activityStatus);
                break;
            case PUBLISH:
                mRoomState.liveMode.set(liveInfo.isPublicVisible ? PUBLIC : PRIVACY);
                break;
            case CATEGORY:
                if (liveInfo.categoryList != null && !liveInfo.categoryList.isEmpty()) {
                    RoomState.LiveCategory category = RoomState.LiveCategory.getCategory(liveInfo.categoryList.get(0));
                    if (category != null) {
                        mRoomState.category.set(category.name());
                    }
                }
                break;
            default:
                break;
        }
    }

    public void onLiveEnd(String roomId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.livekit_room_destroy));
        updateLiveStatus(DASHBOARD);
    }
}
