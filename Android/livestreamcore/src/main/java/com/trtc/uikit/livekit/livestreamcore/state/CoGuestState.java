package com.trtc.uikit.livekit.livestreamcore.state;

import static com.trtc.uikit.livekit.livestreamcore.state.CoGuestState.CoGuestStatus.NONE;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.Request;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatInfo;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class CoGuestState {
    public LiveData<CoGuestStatus>          coGuestStatus         = new LiveData<>(NONE);
    public LiveData<List<SeatInfo>>         connectedUserList     = new LiveData<>(new CopyOnWriteArrayList<>());
    public LiveData<LinkedHashSet<Request>> connectionRequestList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<String>                 myRequestId           = new LiveData<>("");
    public boolean                          openCameraOnCoGuest   = true;
    public boolean                          enableConnection      = true;

    public void reset() {
        coGuestStatus.set(NONE);
        connectedUserList.get().clear();
        connectionRequestList.get().clear();
        myRequestId.set("");
    }

    @NonNull
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        for (SeatInfo guestUser : connectedUserList.get()) {
            builder.append("[");
            builder.append(guestUser.index);
            builder.append(",");
            builder.append(guestUser.userId);
            builder.append(",");
            builder.append(guestUser.isLocked ? "1" : "0");
            builder.append(",");
            builder.append(guestUser.isAudioLocked ? "1" : "0");
            builder.append("]");
        }
        builder.append("}");
        return builder.toString();
    }

    public enum CoGuestStatus {
        NONE,
        APPLYING,
        LINKING
    }
}
