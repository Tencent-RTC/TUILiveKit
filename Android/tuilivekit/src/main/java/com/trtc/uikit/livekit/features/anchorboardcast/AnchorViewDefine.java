package com.trtc.uikit.livekit.features.anchorboardcast;

public class AnchorViewDefine {

    public static class AnchorState {
        public long viewCount;
        public long duration;
        public long messageCount;
        public long giftIncome;
        public long giftSenderCount;
        public long likeCount;
    }

    public interface AnchorViewListener {
        void onEndLiving(AnchorState state);

        void onClickFloatWindow();
    }

    public enum RoomBehavior {
        CREATE_ROOM,
        ENTER_ROOM;
    }
}
