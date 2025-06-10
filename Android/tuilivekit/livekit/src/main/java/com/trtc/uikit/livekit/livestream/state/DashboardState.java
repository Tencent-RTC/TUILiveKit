package com.trtc.uikit.livekit.livestream.state;

import androidx.lifecycle.MutableLiveData;

import java.util.HashSet;
import java.util.Set;

public class DashboardState {
    public long                     duration        = 0;
    public MutableLiveData<Integer> maxViewersCount = new MutableLiveData<>(0);
    public int                      messageCount    = 0;
    public int                      giftIncome      = 0;
    public Set<String>              giftPeopleSet   = new HashSet<>();
    public int                      likeCount       = 0;

    public void reset() {
        duration = 0;
        maxViewersCount.setValue(0);
        messageCount = 0;
        giftIncome = 0;
        giftPeopleSet.clear();
        likeCount = 0;
    }

    @Override
    public String toString() {
        return "DashboardState{" +
                "duration=" + duration +
                ", maxViewersCount=" + maxViewersCount +
                ", messageCount=" + messageCount +
                ", giftIncome=" + giftIncome +
                ", likeCount=" + likeCount +
                ", giftPeopleSet.size=" + giftPeopleSet.size() +
                '}';
    }
}
