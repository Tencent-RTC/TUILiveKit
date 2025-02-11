package com.trtc.uikit.livekit.livestream.state;

import com.trtc.tuikit.common.livedata.LiveData;

import java.util.HashSet;
import java.util.Set;

public class DashboardState {
    public long              duration        = 0;
    public LiveData<Integer> maxViewersCount = new LiveData<>(0);
    public int               messageCount    = 0;
    public int               giftIncome      = 0;
    public Set<String>       giftPeopleSet   = new HashSet<>();
    public int               likeCount       = 0;

    public void reset() {
        duration = 0;
        maxViewersCount.set(0, false);
        messageCount = 0;
        giftIncome = 0;
        giftPeopleSet.clear();
        likeCount = 0;
    }
}
