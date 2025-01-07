package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

import java.util.Map;

public class UserManager extends BaseManager {
    public UserManager(VoiceRoomState state, IVoiceRoomService service, SeatGridViewObserverManager observerManager) {
        super(state, service, observerManager);
        initSelfUserData();
    }

    @Override
    public void destroy() {

    }

    public void updateOwnerUserInfo() {
        if (TextUtils.isEmpty(mRoomState.ownerId)) {
            return;
        }
        if (mRoomState.ownerId.equals(mUserState.selfInfo.userId)) {
            mUserState.selfInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
        }
    }

    private void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mUserState.selfInfo.userId = loginUserInfo.userId;
        mUserState.selfInfo.userName = loginUserInfo.userName;
        mUserState.selfInfo.avatarUrl = loginUserInfo.userName;
        mUserState.selfInfo.userRole = TUIRoomDefine.Role.GENERAL_USER;
    }

    /******************************************  Observer *******************************************/
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        if (hasAudio) {
            mUserState.hasAudioStreamUserList.add(userId);
        } else {
            mUserState.hasAudioStreamUserList.remove(userId);
        }
        getUserInfo(userId, user -> {
            for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
                observer.onUserAudioStateChanged(user, hasAudio, reason);
            }
        });
    }

    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        for (Map.Entry<String, Integer> entry : volumeMap.entrySet()) {
            String userId = entry.getKey();
            UserState.UserVolume newUserVolume = new UserState.UserVolume(userId);
            UserState.UserVolume userVolume = mUserState.userVolumeList.find(newUserVolume);
            if (userVolume != null) {
                userVolume.volume = entry.getValue();
                mUserState.userVolumeList.change(userVolume);
            } else {
                newUserVolume.volume = entry.getValue();
                mUserState.userVolumeList.add(newUserVolume);
            }
        }
    }
    /*************************************************************************************/
}
