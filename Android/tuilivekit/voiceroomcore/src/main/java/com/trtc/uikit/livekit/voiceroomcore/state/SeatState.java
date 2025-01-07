package com.trtc.uikit.livekit.voiceroomcore.state;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatInfo;
import com.trtc.tuikit.common.livedata.LiveListData;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class SeatState {
    public LiveListData<Seat>                 seatList               =
            new LiveListData<>(new CopyOnWriteArrayList<>());
    public Map<String, TUIRoomDefine.Request> seatApplicationMap     = new HashMap<>();
    public String                             mySeatApplicationId    = "";
    public Map<String, TUIRoomDefine.Request> sentSeatInvitationMap  = new HashMap<>();
    public TUIRoomDefine.Request              receivedSeatInvitation = new TUIRoomDefine.Request();
    public Map<String, Seat>                  seatUserMap            = new HashMap<>();

    public void reset() {
        seatUserMap.clear();
        seatList.clear();
        seatApplicationMap.clear();
        receivedSeatInvitation.userId = "";
        mySeatApplicationId = "";
    }

    @NonNull
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        for (Seat seat : seatList.getList()) {
            if (seat == null) {
                continue;
            }
            if (seat.seatInfo == null) {
                continue;
            }
            SeatInfo seatInfo = seat.seatInfo;
            builder.append("[SeatInfo index=")
                    .append(seatInfo.index)
                    .append(", userId=")
                    .append(seatInfo.userId)
                    .append(", isLocked=")
                    .append(seatInfo.isLocked ? "1" : "0")
                    .append(", isAudioLocked=")
                    .append(seatInfo.isAudioLocked ? "1" : "0")
                    .append("]");
        }
        return builder.toString();
    }

    public static class Seat {
        public SeatInfo seatInfo;
        public int      rowIndex;
        public int      columnIndex;

        public Seat update(TUIRoomDefine.SeatInfo seatInfo) {
            if (this.seatInfo == null) {
                this.seatInfo = new SeatInfo();
            }
            this.seatInfo.isAudioLocked = seatInfo.isAudioLocked;
            this.seatInfo.isLocked = seatInfo.isLocked;
            this.seatInfo.userId = seatInfo.userId;
            this.seatInfo.userName = seatInfo.userName;
            this.seatInfo.avatarUrl = seatInfo.avatarUrl;
            return this;
        }
    }
}
