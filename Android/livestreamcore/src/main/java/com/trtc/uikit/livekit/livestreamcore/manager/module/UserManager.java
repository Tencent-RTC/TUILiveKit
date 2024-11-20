package com.trtc.uikit.livekit.livestreamcore.manager.module;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

public class UserManager extends BaseManager {
    private static final String TAG = "UserManager";

    public UserManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        initSelfUserData();
    }

    @Override
    public void destroy() {
        Logger.info(TAG + " destroy");
    }

    public void updateOwnerInfo(TUIRoomDefine.RoomInfo roomInfo) {
        mVideoLiveState.roomState.ownerInfo.userId = roomInfo.ownerId;
        mVideoLiveState.roomState.ownerInfo.userName = roomInfo.ownerName;
        mVideoLiveState.roomState.ownerInfo.avatarUrl = roomInfo.ownerAvatarUrl;
    }

    public void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mVideoLiveState.userState.selfInfo.userId = loginUserInfo.userId;
        mVideoLiveState.userState.selfInfo.userName = loginUserInfo.userName;
        mVideoLiveState.userState.selfInfo.avatarUrl = loginUserInfo.avatarUrl;
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

    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        if (hasAudio) {
            mVideoLiveState.userState.hasAudioStreamUserList.add(userId);
        } else {
            mVideoLiveState.userState.hasAudioStreamUserList.remove(userId);
        }
        if (userId.equals(mVideoLiveState.userState.selfInfo.userId)) {
            updateLocalMicrophoneState(hasAudio);
        }
    }

    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        if (hasVideo) {
            mVideoLiveState.userState.hasVideoStreamUserList.add(userId);
        } else {
            mVideoLiveState.userState.hasVideoStreamUserList.remove(userId);
        }
        if (userId.equals(mVideoLiveState.userState.selfInfo.userId)) {
            updateLocalCameraState(hasVideo);
        }
    }
}
