package com.trtc.uikit.livekit.livestream.manager.observer;

import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;

import java.lang.ref.WeakReference;
import java.util.List;

public class IMFriendshipListener extends V2TIMFriendshipListener {
    protected WeakReference<LiveStreamManager> mLiveManager;

    public IMFriendshipListener(LiveStreamManager liveStreamManager) {
        mLiveManager = new WeakReference<>(liveStreamManager);
    }

    @Override
    public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
        LiveStreamManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onMyFollowingListChanged(userInfoList, isAdd);
        }
    }
}
