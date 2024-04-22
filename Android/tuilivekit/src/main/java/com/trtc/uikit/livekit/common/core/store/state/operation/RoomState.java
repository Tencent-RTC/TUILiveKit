package com.trtc.uikit.livekit.common.core.store.state.operation;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.utils.Constants;

import java.util.HashSet;
import java.util.Set;

public class RoomState {
    public String                                       roomId            = "";
    public long                                         createTime        = 0;
    public LiveData<String>                             ownerId           = new LiveData<>("");
    public LiveData<String>                             roomName          = new LiveData<>("");
    public LiveData<String>                             coverURL          =
            new LiveData<>(Constants.DEFAULT_COVER_URL);
    public LiveData<String>                             category          = new LiveData<>("");
    public LiveData<LiveDefine.LiveStreamPrivacyStatus> liveMode          =
            new LiveData<>(LiveDefine.LiveStreamPrivacyStatus.PUBLIC);
    public LiveData<TUIRoomDefine.SeatMode>             seatMode          =
            new LiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public LiveData<Integer>                            audienceCount     = new LiveData<>(0);
    public LiveData<Integer>                            maxSeatCount      = new LiveData<>(0);
    public LiveData<EnterRoomSate>                      enterRoomState    = new LiveData<>(EnterRoomSate.NOT_ENTERED);
    public int                                          maxAudienceNumber = 0;
    public int                                          messageCount      = 0;
    public int                                          giftIncome        = 0;
    public Set<String>                                  giftPeopleSet     = new HashSet<>();
    public int                                          likeNumber        = 0;

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        this.createTime = roomInfo.createTime;
        this.roomName.set(roomInfo.name);
        this.seatMode.set(roomInfo.seatMode);
        this.ownerId.set(roomInfo.ownerId);
    }

    public enum EnterRoomSate {
        NOT_ENTERED,
        IN_ROOM
    }
}
