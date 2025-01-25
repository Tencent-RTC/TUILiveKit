package com.trtc.uikit.livekit.livestreamcore.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

import java.util.List;

public class UserManager extends BaseManager {
    private static final String TAG = "UserManager";

    private UserInfoObserver mUserInfoObserver;

    public UserManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        initSelfUserData();
    }

    public void setUserInfoObserver(UserInfoObserver observer) {
        mUserInfoObserver = observer;
    }

    @Override
    public void destroy() {
        Logger.info(TAG + " destroy");
    }

    public void updateOwnerInfo(TUIRoomDefine.RoomInfo roomInfo) {
        mVideoLiveState.roomState.ownerInfo.userId = roomInfo.ownerId;
        mVideoLiveState.roomState.ownerInfo.userName = roomInfo.ownerName;
        mVideoLiveState.roomState.ownerInfo.avatarUrl = roomInfo.ownerAvatarUrl;
        if (TextUtils.isEmpty(roomInfo.ownerId)) {
            return;
        }
        if (roomInfo.ownerId.equals(mVideoLiveState.userState.selfInfo.userId)) {
            mVideoLiveState.userState.selfInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
            mVideoLiveState.roomState.ownerInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
        }
    }

    public void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mVideoLiveState.userState.selfInfo.userId = loginUserInfo.userId;
        mVideoLiveState.userState.selfInfo.userName = loginUserInfo.userName;
        mVideoLiveState.userState.selfInfo.avatarUrl = loginUserInfo.avatarUrl;
    }

    public boolean isSelf(String userId) {
        return TextUtils.equals(userId, mVideoLiveState.userState.selfInfo.userId);
    }

    private void updateLocalMicrophoneState(boolean hasAudio) {
        mVideoLiveState.mediaState.isMicrophoneMuted.set(!hasAudio);
        if (hasAudio) {
            mVideoLiveState.mediaState.isMicrophoneOpened.set(true);
        }
    }

    private void updateLocalCameraState(boolean hasVideo) {
        mVideoLiveState.mediaState.isCameraOpened.set(hasVideo);
    }

    public interface UserInfoObserver {
        void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason);

        void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo,
                                     TUIRoomDefine.ChangeReason reason);

        void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag);
    }

    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        if (hasAudio) {
            mVideoLiveState.userState.hasAudioStreamUserList.add(userId);
        } else {
            mVideoLiveState.userState.hasAudioStreamUserList.remove(userId);
        }
        if (userId.equals(mVideoLiveState.userState.selfInfo.userId)) {
            updateLocalMicrophoneState(hasAudio);
        }
        if (mUserInfoObserver != null) {
            mUserInfoObserver.onUserAudioStateChanged(userId, hasAudio, reason);
        }
    }

    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        if (hasVideo) {
            mVideoLiveState.userState.hasVideoStreamUserList.add(userId);
        } else {
            mVideoLiveState.userState.hasVideoStreamUserList.remove(userId);
        }
        if (isSelf(userId)) {
            updateLocalCameraState(hasVideo);
        }

        if (mUserInfoObserver != null) {
            mUserInfoObserver.onUserVideoStateChanged(userId, streamType, hasVideo, reason);
        }
    }

    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        if (mUserInfoObserver != null) {
            mUserInfoObserver.onUserInfoChanged(userInfo, modifyFlag);
        }
    }
}
