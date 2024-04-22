package com.trtc.uikit.livekit.common.core;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;

import java.io.Serializable;

public class LiveDefine {
    public static final int DEFAULT_MAX_SEAT_COUNT = 8;

    public static class RoomParams implements Serializable {
        public String                 roomName     = "";
        public int                    maxSeatCount = DEFAULT_MAX_SEAT_COUNT;
        public TUIRoomDefine.SeatMode seatMode     = TUIRoomDefine.SeatMode.FREE_TO_TAKE;
    }

    public enum RoomBehavior {
        AUTO_CREATE,
        PREPARE_CREATE,
        JOIN
    }

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.livekit_stream_privacy_status_default),
        PRIVACY(R.string.livekit_stream_privacy_status_privacy);

        public final int resId;

        LiveStreamPrivacyStatus(Integer id) {
            this.resId = id;
        }
    }
}