package com.trtc.uikit.livekit.features.endstatistics.state;

import androidx.lifecycle.MutableLiveData;

public class EndStatisticsState {
    public final MutableLiveData<String>  roomId          = new MutableLiveData<>("");
    public final MutableLiveData<String>  ownerName       = new MutableLiveData<>("");
    public final MutableLiveData<String>  ownerAvatarUrl  = new MutableLiveData<>("");
    public final MutableLiveData<Long>    liveDurationMS  = new MutableLiveData<>(0L);
    public final MutableLiveData<Long>    maxViewersCount = new MutableLiveData<>(0L);
    public final MutableLiveData<Long>    messageCount    = new MutableLiveData<>(0L);
    public final MutableLiveData<Long>    likeCount       = new MutableLiveData<>(0L);
    public final MutableLiveData<Long>    giftIncome      = new MutableLiveData<>(0L);
    public final MutableLiveData<Long>    giftSenderCount = new MutableLiveData<>(0L);
    public final MutableLiveData<Boolean> exitClick       = new MutableLiveData<>(false);

    @Override
    public String toString() {
        return "EndStatisticsState{" +
                "roomId=" + roomId.getValue() +
                ", ownerName=" + ownerName.getValue() +
                ", ownerAvatarUrl=" + ownerAvatarUrl.getValue() +
                ", liveDurationMS=" + liveDurationMS.getValue() +
                ", maxViewersCount=" + maxViewersCount.getValue() +
                ", messageCount=" + messageCount.getValue() +
                ", likeCount=" + likeCount.getValue() +
                ", giftIncome=" + giftIncome.getValue() +
                ", giftSenderCount=" + giftSenderCount.getValue() +
                ", exitClick=" + exitClick.getValue() +
                '}';
    }
}
