package com.trtc.uikit.livekit.livestream.state;


import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.NONE;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class CoGuestState {
    public LiveData<List<SeatInfo>>                 connectedUserList  = new LiveData<>(new CopyOnWriteArrayList<>());
    public LiveData<LinkedHashSet<SeatApplication>> requestCoGuestList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<CoGuestStatus>                  coGuestStatus      = new LiveData<>(NONE);

    public void reset() {
        connectedUserList.get().clear();
        requestCoGuestList.get().clear();
        coGuestStatus.set(NONE);
    }


    public static class SeatInfo {
        public LiveData<String> userId    = new LiveData<>("");
        public LiveData<String> name      = new LiveData<>("");
        public LiveData<String> avatarUrl = new LiveData<>("");

        public SeatInfo() {
        }

        public SeatInfo(String userId) {
            this.userId.set(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + ", userId=" + userId.get()
                    + '}';
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof SeatInfo) {
                return this.userId.get().equals(((SeatInfo) obj).userId.get());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(userId.get());
        }

        public void updateState(TUIRoomDefine.UserInfo seatInfo) {
            this.userId.set(seatInfo.userId, false);
            if (!TextUtils.isEmpty(seatInfo.userName)) {
                this.name.set(seatInfo.userName, false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(seatInfo.avatarUrl, false);
        }

        public void updateState(SeatInfo newSeatInfo) {
            this.userId.set(newSeatInfo.userId.get());
            if (!TextUtils.isEmpty(newSeatInfo.name.get())) {
                this.name.set(newSeatInfo.name.get(), false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(newSeatInfo.avatarUrl.get(), false);
        }
    }

    public static class SeatApplication {
        public String userId    = "";
        public String userName  = "";
        public String avatarUrl = "";
        public long   timestamp;

        public SeatApplication(String userId) {
            this.userId = userId;
        }

        public void updateState(TUIRoomDefine.UserInfo liveUser) {
            this.avatarUrl = liveUser.avatarUrl;
            this.userId = liveUser.userId;
            if (!TextUtils.isEmpty(liveUser.userName)) {
                this.userName = liveUser.userName;
            } else {
                this.userName = liveUser.userId;
            }
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (TextUtils.isEmpty(this.userId)) {
                return false;
            }
            if (obj instanceof SeatApplication) {
                return this.userId.equals(((SeatApplication) obj).userId);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatApplication{"
                    + ", userId='" + userId + '\''
                    + ", timestamp=" + timestamp
                    + '}';
        }
    }

    public enum CoGuestStatus {
        NONE,
        APPLYING,
        LINKING
    }

}
