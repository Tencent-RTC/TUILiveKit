package com.trtc.uikit.livekit.livestream.manager.module;

import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.state.LiveState;

public class DashboardManager extends BaseManager {

    public DashboardManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    @Override
    protected void destroy() {

    }

    public void updateDuration(long duration) {
        mDashboardState.duration = duration;
    }

    public void updateMaxViewersCount(int count) {
        mDashboardState.maxViewersCount = count;
    }

    public void updateGiftIncome(int giftIncome) {
        mDashboardState.giftIncome = giftIncome;
    }

    public void insertGiftPeople(String userId) {
        mDashboardState.giftPeopleSet.add(userId);
    }

    public void updateMessageCount(int messageCount) {
        mDashboardState.messageCount = messageCount;
    }

    public void updateLikeNumber(int messageCount) {
        mDashboardState.likeCount = messageCount;
    }

}
