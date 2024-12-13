package com.trtc.uikit.livekit.livestream.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.state.LiveState;
import com.trtc.uikit.livekit.livestream.state.UserState;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class UserManager extends BaseManager {
    private static final String TAG                        = "UserManager";
    private static final int    VOLUME_CAN_HEARD_MIN_LIMIT = 25;

    public UserManager(LiveState state, ILiveService service) {
        super(state, service);
        initSelfUserData();
    }

    @Override
    public void destroy() {
    }

    public void getAudienceList() {
        mLiveService.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mUserState.userList.get().clear();
                    Set<UserState.UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
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
                ErrorHandler.onError(error);
            }
        });
    }

    public void muteAllRemoteAudio(boolean isMute) {
        mLiveService.muteAllRemoteAudio(isMute);
    }

    public void updateOwnerUserInfo() {
        String ownerId = mRoomState.ownerInfo.userId;
        if (TextUtils.isEmpty(ownerId)) {
            return;
        }
        if (ownerId.equals(mUserState.selfInfo.userId)) {
            mUserState.selfInfo.role.set(TUIRoomDefine.Role.ROOM_OWNER);
        }
        mLiveService.getUserInfo(ownerId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mRoomState.ownerInfo.updateState(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mUserState.selfInfo.userId = loginUserInfo.userId;
        mUserState.selfInfo.name.set(loginUserInfo.userName);
        mUserState.selfInfo.avatarUrl.set(loginUserInfo.avatarUrl);
    }

    private void updateLocalMicrophoneState(boolean hasAudio) {
        mMediaState.isMicrophoneMuted.set(!hasAudio);
        if (hasAudio) {
            mMediaState.isMicrophoneOpened.set(true);
        }
    }

    private void updateLocalCameraState(boolean hasVideo) {
        mMediaState.isCameraOpened.set(hasVideo);
    }

    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        if (hasAudio) {
            mUserState.hasAudioStreamUserList.add(userId);
        } else {
            mUserState.hasAudioStreamUserList.remove(userId);
        }
        if (userId.equals(mUserState.selfInfo.userId)) {
            updateLocalMicrophoneState(hasAudio);
        }
    }

    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        if (hasVideo) {
            mUserState.hasVideoStreamUserList.add(userId);
        } else {
            mUserState.hasVideoStreamUserList.remove(userId);
        }
        if (userId.equals(mUserState.selfInfo.userId)) {
            updateLocalCameraState(hasVideo);
        }
    }

    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        for (Map.Entry<String, Integer> entry : volumeMap.entrySet()) {
            String userId = entry.getKey();
            if (entry.getValue() > VOLUME_CAN_HEARD_MIN_LIMIT) {
                mUserState.speakingUserList.add(userId);
            } else {
                mUserState.speakingUserList.remove(userId);
            }
        }
    }

    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            return;
        }
        mUserState.addUser(userInfo);
    }

    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        mUserState.removeUser(userInfo);
    }
}
