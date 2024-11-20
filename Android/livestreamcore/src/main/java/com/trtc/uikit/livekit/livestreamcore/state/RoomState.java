package com.trtc.uikit.livekit.livestreamcore.state;

import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.NONE;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.LiveData;

public class RoomState {
    public String               roomId          = "";
    public UserInfo             ownerInfo       = new UserInfo();
    public LiveData<Integer>    maxCoGuestCount = new LiveData<>(0);
    public LiveData<LiveStatus> liveStatus      = new LiveData<>(NONE);

    public void reset() {
        liveStatus.set(NONE);
        maxCoGuestCount.set(0);
    }

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        this.ownerInfo.userId = roomInfo.ownerId;
        this.maxCoGuestCount.set(roomInfo.maxSeatCount, false);
    }

    public enum LiveStatus {
        NONE,
        PUSHING,
        PLAYING,
    }
}
