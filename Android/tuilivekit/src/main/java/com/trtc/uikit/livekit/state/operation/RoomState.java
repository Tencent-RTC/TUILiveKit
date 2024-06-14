package com.trtc.uikit.livekit.state.operation;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.state.LiveDefine;

import java.util.HashSet;
import java.util.Set;

public class RoomState {
    public String                           roomId        = "";
    public long                             createTime    = 0;
    public UserState.UserInfo               ownerInfo     = new UserState.UserInfo();
    public LiveData<String>                 roomName      = new LiveData<>("");
    public LiveData<String>                 coverURL      =
            new LiveData<>(Constants.DEFAULT_COVER_URL);
    public LiveData<TUIRoomDefine.SeatMode> seatMode      =
            new LiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public LiveData<Integer>                userCount     = new LiveData<>(0);
    public LiveData<Integer>                maxSeatCount  = new LiveData<>(0);
    public LiveExtraInfo                    liveExtraInfo = new LiveExtraInfo();

    public void reset() {
        createTime = 0;
        roomName.set("");
        coverURL.set(Constants.DEFAULT_COVER_URL);
        seatMode.set(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
        userCount.set(0);
        maxSeatCount.set(0);
        liveExtraInfo = new LiveExtraInfo();
    }

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        this.createTime = roomInfo.createTime;
        this.roomName.set(roomInfo.name, false);
        this.seatMode.set(roomInfo.seatMode, false);
        this.ownerInfo.userId = roomInfo.ownerId;
        this.maxSeatCount.set(roomInfo.maxSeatCount, false);
    }

    public static class LiveExtraInfo {
        public LiveData<String>                             category         = new LiveData<>("");
        public LiveData<LiveDefine.LiveStreamPrivacyStatus> liveMode         =
                new LiveData<>(LiveDefine.LiveStreamPrivacyStatus.PUBLIC);
        public int                                          maxAudienceCount = 0;
        public int                                          messageCount     = 0;
        public int                                          giftIncome       = 0;
        public Set<String>                                  giftPeopleSet    = new HashSet<>();
        public int                                          likeCount        = 0;
    }
}
