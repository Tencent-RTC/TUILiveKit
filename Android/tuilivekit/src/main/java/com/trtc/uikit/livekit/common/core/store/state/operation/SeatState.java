package com.trtc.uikit.livekit.common.core.store.state.operation;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;

public class SeatState {
    public LiveData<List<SeatInfo>>                 seatList            = new LiveData<>(new ArrayList<>());
    public LiveData<LinkedHashSet<SeatApplication>> seatApplicationList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<String>                         mySeatApplicationId = new LiveData<>("");

    @NonNull
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        for (SeatInfo seatInfo : seatList.get()) {
            builder.append("[");
            builder.append(seatInfo.index);
            builder.append(",");
            builder.append(seatInfo.userId.get());
            builder.append(",");
            builder.append(seatInfo.isLocked.get() ? "1" : "0");
            builder.append(",");
            builder.append(seatInfo.isAudioLocked.get() ? "1" : "0");
            builder.append("]");
        }
        builder.append("}");
        return builder.toString();
    }

    public void initSeatList(List<TUIRoomDefine.SeatInfo> list) {
        this.seatList.get().clear();
        List<SeatInfo> newList = new ArrayList<>();
        for (TUIRoomDefine.SeatInfo info : list) {
            SeatInfo seatInfo = new SeatInfo();
            seatInfo.updateState(info);
            newList.add(seatInfo);
        }
        this.seatList.addAll(newList);
    }

    public void updateSeatList(List<TUIRoomDefine.SeatInfo> list) {
        if (list.size() != this.seatList.get().size()) {
            initSeatList(list);
            return;
        }
        for (int i = 0; i < list.size(); i++) {
            SeatInfo oldSeatInfo = this.seatList.get().get(i);
            TUIRoomDefine.SeatInfo newSeatInfo = list.get(i);
            if (oldSeatInfo == null || newSeatInfo == null) {
                continue;
            }
            oldSeatInfo.updateState(newSeatInfo);
        }
    }

    public void initSeatApplicationList(List<TUIRoomDefine.Request> list) {
        seatApplicationList.get().clear();
        List<SeatApplication> newList = new ArrayList<>();
        for (TUIRoomDefine.Request request : list) {
            SeatApplication application = new SeatApplication(request.requestId);
            application.updateState(request);
            newList.add(application);
        }
        seatApplicationList.addAll(newList);
    }

    public void addSeatApplication(TUIRoomDefine.Request request) {
        if (request == null) {
            return;
        }
        if (TextUtils.isEmpty(request.requestId)) {
            return;
        }
        if (request.requestAction != TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
            return;
        }
        SeatApplication seatApplication = new SeatApplication(request.requestId);
        seatApplication.updateState(request);
        seatApplicationList.add(seatApplication);
    }

    public void removeSeatApplication(String id) {
        SeatApplication application = new SeatApplication(id);
        seatApplicationList.remove(application);
    }

    public static class SeatInfo {
        public int               index         = 0;
        public LiveData<String>  userId        = new LiveData<>("");
        public LiveData<String>  name          = new LiveData<>("");
        public LiveData<String>  avatarUrl     = new LiveData<>("");
        public LiveData<Boolean> isLocked      = new LiveData<>(false);
        public LiveData<Boolean> isAudioLocked = new LiveData<>(false);

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + "index=" + index
                    + ", userId=" + userId.get()
                    + ", isLocked=" + (isLocked.get() ? "1" : "0")
                    + ", isAudioLocked=" + (isAudioLocked.get() ? "1" : "0")
                    + '}';
        }

        public void updateState(TUIRoomDefine.SeatInfo seatInfo) {
            this.index = seatInfo.index;
            this.userId.set(seatInfo.userId, false);
            if (!TextUtils.isEmpty(seatInfo.userName)) {
                this.name.set(seatInfo.userName, false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(seatInfo.avatarUrl, false);
            this.isLocked.set(seatInfo.isLocked, false);
            this.isAudioLocked.set(seatInfo.isAudioLocked, false);
        }
    }

    public static class SeatApplication {
        public String id        = "";
        public String userId    = "";
        public String userName  = "";
        public String avatarUrl = "";
        public int    timestamp;

        public SeatApplication(String id) {
            this.id = id;
        }

        public void updateState(TUIRoomDefine.Request request) {
            this.userName = request.userName;
            this.avatarUrl = request.avatarUrl;
            this.userId = request.userId;
            this.timestamp = request.timestamp;
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof SeatApplication) {
                return this.id.equals(((SeatApplication) obj).id);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(id);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatApplication{"
                    + "id='" + id + '\''
                    + ", userId='" + userId + '\''
                    + ", timestamp=" + timestamp
                    + '}';
        }
    }
}
