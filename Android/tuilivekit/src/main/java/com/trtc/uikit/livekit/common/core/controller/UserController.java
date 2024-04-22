package com.trtc.uikit.livekit.common.core.controller;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.common.core.error.ErrorHandler;
import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class UserController extends Controller {
    private static final String TAG                        = "UserController";
    private static final int    VOLUME_CAN_HEARD_MIN_LIMIT = 25;

    public UserController(LiveState state, RoomEngineService service) {
        super(state, service);
        mRoomEngineService.addObserver(mObserver);
        initSelfUserData();
    }

    @Override
    public void destroy() {
        mRoomEngineService.removeObserver(mObserver);
    }

    public void getAudienceList() {
        LiveKitLog.info(TAG + "getAudienceList");
        mRoomEngineService.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                LiveKitLog.info(TAG + " getAudienceList:[Success, " + "userListResult:" + userListResult + "]");
                if (!userListResult.userInfoList.isEmpty()) {
                    mUserState.audienceList.get().clear();
                    Set<UserState.UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mRoomState.ownerId.get())) {
                            continue;
                        }
                        UserState.UserInfo liveUserInfo = new UserState.UserInfo(userInfo);
                        userInfoSet.add(liveUserInfo);
                    }
                    mUserState.addUserList(userInfoSet);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " getAudienceList:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    private void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mUserState.selfInfo.userId = loginUserInfo.userId;
        mUserState.selfInfo.name.set(loginUserInfo.userName);
        mUserState.selfInfo.avatarUrl.set(loginUserInfo.avatarUrl);
    }

    private final TUIRoomObserver mObserver = new TUIRoomObserver() {
        @Override
        public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
            LiveKitLog.info(TAG + " onUserAudioStateChanged userId:" + userId + ",hasAudio:" + hasAudio + ",reason:"
                    + reason);
            if (hasAudio) {
                mUserState.hasAudioStreamUserList.add(userId);
            } else {
                mUserState.hasAudioStreamUserList.remove(userId);
            }
            if (userId.equals(mUserState.selfInfo.userId)) {
                updateLocalMediaState(hasAudio);
            }
        }

        @Override
        public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
            for (Map.Entry<String, Integer> entry : volumeMap.entrySet()) {
                String userId = entry.getKey();
                if (entry.getValue() > VOLUME_CAN_HEARD_MIN_LIMIT) {
                    mUserState.hasAudioVolumeUserList.add(userId);
                } else {
                    mUserState.hasAudioVolumeUserList.remove(userId);
                }
            }
        }

        @Override
        public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            LiveKitLog.info(TAG + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
            if (userInfo.userId.equals(mRoomState.ownerId.get())) {
                return;
            }
            mUserState.addUser(userInfo);
        }

        @Override
        public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
            LiveKitLog.info(TAG + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
            mUserState.removeUser(userInfo);
        }
    };

    private void updateLocalMediaState(boolean hasAudio) {
        mMediaState.isMicrophoneMuted.set(!hasAudio);
        if (hasAudio) {
            mMediaState.isMicrophoneOpened.set(true);
        }
    }

    public void updateOwnerUserInfo() {
        String ownerId = mRoomState.ownerId.get();
        if (TextUtils.isEmpty(ownerId)) {
            return;
        }
        mRoomEngineService.getUserInfo(ownerId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mUserState.ownerInfo.updateState(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " updateOwnerUserInfo:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }
}
