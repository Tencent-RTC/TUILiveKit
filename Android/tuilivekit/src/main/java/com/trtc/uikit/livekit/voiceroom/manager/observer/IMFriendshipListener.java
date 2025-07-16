package com.trtc.uikit.livekit.voiceroom.manager.observer;

import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.List;

public class IMFriendshipListener extends V2TIMFriendshipListener {
    protected VoiceRoomManager mVoiceRoomManager;

    public IMFriendshipListener(VoiceRoomManager voiceRoomManager) {
        mVoiceRoomManager = voiceRoomManager;
    }

    @Override
    public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
        mVoiceRoomManager.getUserManager().onMyFollowingListChanged(userInfoList, isAdd);
    }
}
