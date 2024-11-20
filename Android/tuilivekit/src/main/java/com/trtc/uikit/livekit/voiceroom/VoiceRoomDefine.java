package com.trtc.uikit.livekit.voiceroom;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.io.Serializable;

public class VoiceRoomDefine {
    public static final int MAX_CONNECTED_VIEWERS_COUNT = 10;

    public static class CreateRoomParams implements Serializable {
        public String                 roomName       = "";
        public int                    maxAnchorCount = MAX_CONNECTED_VIEWERS_COUNT;
        public TUIRoomDefine.SeatMode seatMode       = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
    }
}
