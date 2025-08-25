package com.trtc.uikit.livekit.features.endstatistics;

public class EndStatisticsDefine {

    public static class AnchorEndStatisticsInfo {
        public String roomId          = "";
        public long   liveDurationMS  = 0;
        public long   maxViewersCount = 0;
        public long   messageCount    = 0;
        public long   likeCount       = 0;
        public long   giftIncome      = 0;
        public long   giftSenderCount = 0;
    }

    public interface AnchorEndStatisticsViewListener {
        void onCloseButtonClick();
    }

    public interface AudienceEndStatisticsViewListener {
        void onCloseButtonClick();
    }
}
