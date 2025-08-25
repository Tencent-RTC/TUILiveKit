package com.trtc.uikit.livekit.features.endstatistics.manager;

import com.trtc.uikit.livekit.features.endstatistics.state.EndStatisticsState;

import java.util.Locale;

public class EndStatisticsManager {
    private final EndStatisticsState mInternalState = new EndStatisticsState();

    public EndStatisticsManager() {

    }

    public EndStatisticsState getState() {
        return mInternalState;
    }

    public void setRoomId(String roomId) {
        mInternalState.roomId.setValue(roomId);
    }

    public void setOwnerName(String ownerName) {
        mInternalState.ownerName.setValue(ownerName);
    }

    public void setOwnerAvatarUrl(String ownerAvatarUrl) {
        mInternalState.ownerAvatarUrl.setValue(ownerAvatarUrl);
    }

    public void setLiveDuration(long duration) {
        mInternalState.liveDurationMS.setValue(duration);
    }

    public void setMaxViewersCount(long count) {
        mInternalState.maxViewersCount.setValue(count);
    }

    public void setMessageCount(long count) {
        mInternalState.messageCount.setValue(count);
    }

    public void setLikeCount(long count) {
        mInternalState.likeCount.setValue(count);
    }

    public void setGiftIncome(long count) {
        mInternalState.giftIncome.setValue(count);
    }

    public void setGiftSenderCount(long count) {
        mInternalState.giftSenderCount.setValue(count);
    }

    public String formatSeconds(int timeSeconds) {
        String timeString = "-- --";
        if (timeSeconds > 0) {
            int hour = timeSeconds / 3600;
            int min = timeSeconds % 3600 / 60;
            int sec = timeSeconds % 60;
            timeString = String.format(Locale.getDefault(), "%02d:%02d:%02d", hour, min, sec);
        }
        return timeString;
    }
}
