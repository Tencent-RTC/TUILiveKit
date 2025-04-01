package com.trtc.uikit.livekit.livestream.manager.observer;

import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.util.List;

public class IMFriendshipListener extends V2TIMFriendshipListener {
    private final LiveStreamManager mLiveManager;

    public IMFriendshipListener(LiveStreamManager liveStreamManager) {
        mLiveManager = liveStreamManager;
    }

    @Override
    public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
        mLiveManager.getUserManager().onMyFollowingListChanged(userInfoList, isAdd);
    }
}
