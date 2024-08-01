package com.trtc.uikit.livekit.manager.controller;

import static com.trtc.uikit.livekit.common.utils.Constants.DEFAULT_MAX_SEAT_COUNT;
import static com.trtc.uikit.livekit.state.LiveDefine.LiveStreamPrivacyStatus.PUBLIC;

import android.text.TextUtils;
import android.util.Log;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.LiveState;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RoomController extends Controller {
    private static final String TAG = "RoomController";

    private final List<Listener> mListenerList;
    private final List<String>   mCategoryList = new ArrayList<>();

    public RoomController(LiveState state, ILiveService service) {
        super(state, service);
        mListenerList = new ArrayList<>();
    }

    @Override
    public void destroy() {
        LiveKitLog.info(TAG + " destroy");
        mListenerList.clear();
    }

    public void addListener(Listener listener) {
        mListenerList.add(listener);
    }

    public void removeListener(Listener listener) {
        mListenerList.remove(listener);
    }

    public void initCreateRoomState(String roomId, String roomName, TUIRoomDefine.SeatMode seatMode, int maxSeatCount) {
        LiveKitLog.info(TAG + " initCreateRoomState roomId [roomId: " + roomId + ", roomName:" + roomName + ", "
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
        if (maxSeatCount == 0) {
            mSeatState.initSeatList(DEFAULT_MAX_SEAT_COUNT);
        } else {
            mSeatState.initSeatList(maxSeatCount);
        }
    }

    public void start() {
        LiveKitLog.info(TAG + " start[roomId" + mRoomState.roomId + ",liveService:" + mLiveService.hashCode() + "]");
        dataReport();
        if (TextUtils.isEmpty(mRoomState.roomId)) {
            LiveKitLog.error(TAG + " not init create room state");
            return;
        }
        if (TextUtils.isEmpty(mRoomState.roomName.get())) {
            mRoomState.roomName.set(getDefaultRoomName());
        }
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.isSeatEnabled = true;
        roomInfo.roomId = mRoomState.roomId;
        roomInfo.name = mRoomState.roomName.get();
        roomInfo.maxSeatCount = mRoomState.maxSeatCount.get();
        roomInfo.seatMode = mRoomState.seatMode.get();
        mViewState.liveStatus.set(LiveDefine.LiveStatus.PUSHING);
        mLiveService.start(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                updateRoomState(roomInfo);
                updateLiveInfo();
                notifyEnterRoomSuccess();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
                mViewState.liveStatus.set(LiveDefine.LiveStatus.NONE);
            }
        });
    }

    public void join(String roomId) {
        LiveKitLog.info(TAG + " join room [roomId: " + roomId + ",liveService:" + mLiveService.hashCode() + "]");
        dataReport();
        mLiveService.join(roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                getLiveInfo(roomId);
                onEnterRoomSuccess(roomInfo);
                mViewState.liveStatus.set(LiveDefine.LiveStatus.PLAYING);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void getLiveInfo(String roomId) {
        LiveKitLog.info(TAG + " getLiveInfo [roomId: " + roomId + ",liveService:" + mLiveService.hashCode() + "]");
        mLiveService.getLiveInfo(roomId, new TUILiveListManager.LiveInfoCallback() {
            @Override
            public void onSuccess(LiveInfo liveInfo) {
                updateCoverUrl(liveInfo.coverUrl);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void updateCoverUrl(String coverUrl) {
        mRoomState.coverURL.set(coverUrl, false);
    }

    public void exit() {
        LiveKitLog.info(TAG + "[" + mRoomState.roomId + "] exit start");
        if (isOwner()) {
            stop();
        } else {
            leave();
        }
    }

    public void clearLiveState() {
        mViewState.liveStatus.set(LiveDefine.LiveStatus.NONE);
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

    public void startPreview() {
        mViewState.liveStatus.set(LiveDefine.LiveStatus.PREVIEWING);
    }

    public void setRoomName(String roomName) {
        mRoomState.roomName.set(roomName, false);
    }

    public void setCoverURL(String url) {
        mRoomState.coverURL.set(url, false);
    }

    public void setBackgroundURL(String backgroundURL) {
        if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.PREVIEWING) {
            mRoomState.backgroundURL.set(backgroundURL, false);
        }
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

    public void updateRoomSeatMode(TUIRoomDefine.SeatMode seatMode) {
        LiveKitLog.info(TAG + " updateRoomSeatMode [roomId: " + mRoomState.roomId + "]");
        if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.PREVIEWING) {
            mRoomState.seatMode.set(seatMode, false);
            return;
        }
        mLiveService.updateRoomSeatModeByAdmin(seatMode, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " updateRoomSeatMode success");
                mRoomState.seatMode.set(seatMode, false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " updateRoomSeatMode:[Error] error:" + error + ",message:" + message + "]");
            }
        });
    }

    private void leave() {
        LiveKitLog.info(TAG + " leave room [roomId: " + mRoomState.roomId + "]");
        mLiveService.leave(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " leave success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " leave:[Error] error:" + error + ",message:" + message + "]");
            }
        });
    }

    private void stop() {
        LiveKitLog.info(TAG + " stop room [roomId: " + mRoomState.roomId + "]");
        mLiveService.stop(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " stop success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " stop:[Error] error:" + error + ",message:" + message + "]");
            }
        });
    }

    public boolean isOwner() {
        String selfUserId = TUIRoomEngine.getSelfInfo().userId;
        if (TextUtils.isEmpty(selfUserId)) {
            return false;
        }
        return selfUserId.equals(mRoomState.ownerInfo.userId);
    }

    private void onEnterRoomSuccess(TUIRoomDefine.RoomInfo roomInfo) {
        updateRoomState(roomInfo);
        notifyEnterRoomSuccess();
    }

    private void notifyEnterRoomSuccess() {
        for (Listener listener : mListenerList) {
            listener.onEnterRoomSuccess();
        }
    }

    private void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.updateState(roomInfo);
    }

    private void dataReport() {
        try {
            JSONObject params = new JSONObject();
            params.put("framework", Constants.DATA_REPORT_FRAMEWORK);
            params.put("component", Constants.DATA_REPORT_COMPONENT);
            params.put("language", Constants.DATA_REPORT_LANGUAGE_JAVA);

            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "setFramework");
            jsonObject.put("params", params);
            mLiveService.callExperimentalAPI(jsonObject.toString(), null);
        } catch (JSONException e) {
            Log.e(TAG, "dataReport:", e);
        }
    }

    private void updateLiveInfo() {
        LiveInfo liveInfo = new LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = mRoomState.roomId;
        liveInfo.coverUrl = mRoomState.coverURL.get();
        liveInfo.isPublicVisible = PUBLIC == mRoomState.liveExtraInfo.liveMode.get();
        String category = mRoomState.liveExtraInfo.category.get();
        for (int i = 0; i < mCategoryList.size(); i++) {
            String item = mCategoryList.get(i);
            if (TextUtils.equals(item, category) && LiveDefine.LiveCategory.getCategory(i) != null) {
                liveInfo.categoryList = Collections.singletonList(i);
                break;
            }
        }
        List<LiveModifyFlag> flagList = new ArrayList<>();
        flagList.add(LiveModifyFlag.COVER_URL);
        flagList.add(LiveModifyFlag.PUBLISH);
        flagList.add(LiveModifyFlag.CATEGORY);
        mLiveService.setLiveInfo(liveInfo, flagList, null);
    }

    public void onRoomUserCountChanged(String roomId, int userCount) {
        if (userCount > 0) {
            mRoomState.userCount.set(userCount - 1);
            if (userCount > mRoomState.liveExtraInfo.maxAudienceCount) {
                mRoomState.liveExtraInfo.maxAudienceCount = userCount - 1;
            }
        }
    }

    public void onLiveInfoChanged(LiveInfo liveInfo,
                                  List<LiveModifyFlag> modifyFlagList) {
        for (LiveModifyFlag flag : modifyFlagList) {
            if (flag == LiveModifyFlag.COVER_URL) {
                updateCoverUrl(liveInfo.coverUrl);
            }
        }
    }

    public interface Listener {
        void onEnterRoomSuccess();
    }
}
