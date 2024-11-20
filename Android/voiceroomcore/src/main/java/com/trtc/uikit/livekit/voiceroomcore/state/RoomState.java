package com.trtc.uikit.livekit.voiceroomcore.state;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

public class RoomState {
    public String                           roomId       = "";
    public String                           ownerId      = "";
    public LiveData<TUIRoomDefine.SeatMode> seatMode     = new LiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public LiveData<Integer>                maxSeatCount = new LiveData<>(0);

    public void reset() {
        maxSeatCount.set(0);
        ownerId = "";
        roomId = "";
    }

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        this.ownerId = roomInfo.ownerId;
        this.seatMode.set(roomInfo.seatMode);
        this.maxSeatCount.set(roomInfo.maxSeatCount, false);
    }
}
