package com.trtc.uikit.livekit.features.anchorboardcast.manager;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorService;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.impl.AnchorServiceImpl;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.AnchorRoomObserver;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class AnchorManager {
    private final LiveKitLogger               LOGGER = LiveKitLogger.getFeaturesLogger("AnchorManager");
    private final AnchorState                 mInternalState;
    private final IAnchorService              mService;
    private final WeakReference<LiveCoreView> mCoreViewRef;
    private       AnchorRoomObserver          mRoomObserver;

    private static final int FETCH_LIST_COUNT = 20;

    public AnchorManager(LiveCoreView coreView) {
        mService = new AnchorServiceImpl();
        mCoreViewRef = new WeakReference<>(coreView);
        mInternalState = new AnchorState();
        mInternalState.loginUserInfo.userId = TUILogin.getUserId();
        mInternalState.loginUserInfo.userName = TUILogin.getNickName();
        mInternalState.loginUserInfo.avatarUrl = TUILogin.getFaceUrl();

        addObserver();
    }

    private void addObserver() {
        mRoomObserver = new AnchorRoomObserver(this);
        mService.addObserver(mRoomObserver);
    }

    public void release() {
        mService.removeObserver(mRoomObserver);
    }

    public AnchorState getState() {
        return mInternalState;
    }

    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback getUserInfoCallback) {

    }

    public boolean isConnectedConnectionUser(String userId) {
        return false;
    }

    public void initSelfUserData() {
        mInternalState.loginUserInfo = TUIRoomEngine.getSelfInfo();
    }

    public void updateRoomInfo(TUIRoomDefine.RoomInfo roomInfo) {
        mInternalState.roomId = roomInfo.roomId;
        mInternalState.roomName = roomInfo.name;
        mInternalState.createTime = roomInfo.createTime;
        mInternalState.ownerInfo.userId = roomInfo.ownerId;
        mInternalState.ownerInfo.userName = roomInfo.ownerName;
        mInternalState.ownerInfo.avatarUrl = roomInfo.ownerAvatarUrl;
    }

    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mInternalState.ownerInfo.userId)) {
            return;
        }
        mInternalState.enterUserInfo.setValue(userInfo);
    }

    public void joinLiveStream() {
        initSelfUserData();
        mCoreViewRef.get().joinLiveStream(mInternalState.roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                updateRoomInfo(roomInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("joinLiveStream failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void startLiveStream() {
        initSelfUserData();
        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        roomInfo.roomId = mInternalState.roomId;
        roomInfo.name = mInternalState.roomName;
        roomInfo.maxSeatCount = 9;
        mCoreViewRef.get().startLiveStream(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                mCoreViewRef.get().startMicrophone(null);
                updateRoomInfo(roomInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("startLiveStream failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public LiveCoreView getCoreView() {
        return mCoreViewRef.get();
    }

    public LiveCoreViewDefine.CoreState getCoreState() {
        return mCoreViewRef.get().getCoreState();
    }

    public void fetchLiveList() {
        mService.fetchLiveList(mInternalState.recommendedCursor, FETCH_LIST_COUNT,
                new TUILiveListManager.LiveInfoListCallback() {
                    @Override
                    public void onSuccess(TUILiveListManager.LiveInfoListResult result) {
                        List<TUILiveConnectionManager.ConnectionUser> list = mInternalState.recommendUsers.getValue();
                        if (TextUtils.isEmpty(mInternalState.recommendedCursor)) {
                            list.clear();
                        }

                        for (TUILiveListManager.LiveInfo liveInfo : result.liveInfoList) {
                            TUILiveConnectionManager.ConnectionUser user =
                                    new TUILiveConnectionManager.ConnectionUser();

                            user.roomId = liveInfo.roomInfo.roomId;
                            user.userId = liveInfo.roomInfo.ownerId;
                            user.userName = liveInfo.roomInfo.ownerName;
                            user.avatarUrl = liveInfo.roomInfo.ownerAvatarUrl;
                            user.joinConnectionTime = 0;
                            if (!isConnectedConnectionUser(liveInfo.roomInfo.roomId)) {
                                list.add(user);
                            }
                        }
                        mInternalState.recommendedCursor = result.cursor;
                        mInternalState.recommendUsers.setValue(list);
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error("fetchLiveList failed:error:" + error + ",errorCode:" + error.getValue() +
                                "message:" + message);
                        ErrorLocalized.onError(error);
                    }
                });
    }

    public void terminateCrossRoomConnection() {
        mCoreViewRef.get().terminateCrossRoomConnection();
        getState().recommendUsers.setValue(new ArrayList<>());
    }

    public boolean isInvitingConnectionUser(TUILiveConnectionManager.ConnectionUser recommendUser) {
        for (TUILiveConnectionManager.ConnectionUser connectionUser :
                getCoreState().coHostState.sentConnectionRequestList.getValue()) {
            if (TextUtils.equals(connectionUser.roomId, recommendUser.roomId)) {
                return true;
            }
        }
        return false;
    }

    public boolean isConnectedConnectionUser(TUILiveConnectionManager.ConnectionUser recommendUser) {
        for (TUILiveConnectionManager.ConnectionUser connectionUser :
                getCoreState().coHostState.connectedUserList.getValue()) {
            if (TextUtils.equals(connectionUser.roomId, recommendUser.roomId)) {
                return true;
            }
        }
        return false;
    }

    public void onSeatListChanged(List<TUIRoomDefine.UserInfo> userList, List<TUIRoomDefine.UserInfo> joinList,
                                  List<TUIRoomDefine.UserInfo> leaveList) {
    }

    public void onRequestCancelled(TUIRoomDefine.UserInfo inviterUser) {

    }

    public void onUserConnectionRejected(String userId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_voiceroom_take_seat_rejected));
    }

    public void onUserConnectionTimeout(String userId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_voiceroom_take_seat_timeout));
    }

    public void onKickedOffSeat() {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources().getString(R.string.common_voiceroom_kicked_out_of_seat));
    }

    public void onConnectionUserListChanged(List<TUILiveConnectionManager.ConnectionUser> roomList) {
        updateRecommendedList(roomList);
    }

    private void updateRecommendedList(List<TUILiveConnectionManager.ConnectionUser> connectedList) {
        List<TUILiveConnectionManager.ConnectionUser> recommendList = mInternalState.recommendUsers.getValue();
        Iterator<TUILiveConnectionManager.ConnectionUser> iterator = recommendList.iterator();
        while (iterator.hasNext()) {
            TUILiveConnectionManager.ConnectionUser recommendInfo = iterator.next();
            for (TUILiveConnectionManager.ConnectionUser joinedInfo : connectedList) {
                if (TextUtils.equals(recommendInfo.roomId, joinedInfo.roomId)) {
                    iterator.remove();
                }
            }
        }
        mInternalState.recommendUsers.setValue(mInternalState.recommendUsers.getValue());
    }

    public void onConnectionRequestReject(TUILiveConnectionManager.ConnectionUser roomInfo) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_connect_request_rejected));
    }

    public void respondToCrossRoomConnection(String roomId, boolean isAccepted, TUIRoomDefine.ActionCallback callback) {
        mCoreViewRef.get().respondToCrossRoomConnection(roomId, isAccepted, callback);
    }
}
