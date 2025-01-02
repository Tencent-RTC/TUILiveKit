package com.trtc.uikit.livekit.voiceroom.state;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.tuikit.common.livedata.LiveMapData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class SeatState {
    public LiveData<LinkStatus>                     linkStatus             = new LiveData<>(LinkStatus.NONE);
    public LiveData<List<SeatInfo>>                 seatList               =
            new LiveData<>(new CopyOnWriteArrayList<>());
    public LiveData<LinkedHashSet<SeatApplication>> seatApplicationList    =
            new LiveData<>(new LinkedHashSet<>());
    public LiveMapData<String, SeatInvitation>      sentSeatInvitationMap  = new LiveMapData<>(new HashMap<>());
    public LiveData<SeatInvitation>                 receivedSeatInvitation = new LiveData<>(new SeatInvitation(""));

    public void reset() {
        linkStatus.set(LinkStatus.NONE);
        seatList.get().clear();
        seatApplicationList.get().clear();
        sentSeatInvitationMap.get().clear();
        receivedSeatInvitation.get().reset();
    }

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

    public enum LinkStatus {
        NONE,
        APPLYING,
        LINKING
    }

    public static class SeatInfo {
        public int               index         = 0;
        public LiveData<String>  userId        = new LiveData<>("");
        public LiveData<String>  name          = new LiveData<>("");
        public LiveData<String>  avatarUrl     = new LiveData<>("");
        public LiveData<Boolean> isLocked      = new LiveData<>(false);
        public LiveData<Boolean> isAudioLocked = new LiveData<>(false);
        public LiveData<Boolean> isVideoLocked = new LiveData<>(false);

        public SeatInfo() {
        }

        public SeatInfo(String userId) {
            this.userId.set(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + "index=" + index
                    + ", userId=" + userId.get()
                    + ", isLocked=" + (isLocked.get() ? "1" : "0")
                    + ", isAudioLocked=" + (isAudioLocked.get() ? "1" : "0")
                    + ", isVideoLocked=" + (isVideoLocked.get() ? "1" : "0")
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

        public void updateState(SeatInfo newSeatInfo) {
            this.index = newSeatInfo.index;
            this.userId.set(newSeatInfo.userId.get());
            if (!TextUtils.isEmpty(newSeatInfo.name.get())) {
                this.name.set(newSeatInfo.name.get(), false);
            } else {
                this.name.set(this.userId.get(), false);
            }
            this.avatarUrl.set(newSeatInfo.avatarUrl.get(), false);
            this.isLocked.set(newSeatInfo.isLocked.get(), false);
            this.isAudioLocked.set(newSeatInfo.isAudioLocked.get(), false);
        }
    }

    public static class SeatApplication {
        public String userId;
        public String userName  = "";
        public String avatarUrl = "";

        public SeatApplication(String userId) {
            this.userId = userId;
        }

        public void updateState(TUIRoomDefine.UserInfo userInfo) {
            this.avatarUrl = userInfo.avatarUrl;
            if (!TextUtils.isEmpty(userInfo.userName)) {
                this.userName = userInfo.userName;
            } else {
                this.userName = userInfo.userId;
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
                    + '}';
        }
    }

    public static class SeatInvitation {
        public String userId;
        public String userName  = "";
        public String avatarUrl = "";

        public SeatInvitation(String userId) {
            this.userId = userId;
        }

        public void updateState(TUIRoomDefine.UserInfo userInfo) {
            this.avatarUrl = userInfo.avatarUrl;
            if (!TextUtils.isEmpty(userInfo.userName)) {
                this.userName = userInfo.userName;
            } else {
                this.userName = userInfo.userId;
            }
        }

        public void updateState(UserState.UserInfo userInfo) {
            this.avatarUrl = userInfo.avatarUrl.get();
            if (!TextUtils.isEmpty(userInfo.name.get())) {
                this.userName = userInfo.name.get();
            } else {
                this.userName = userInfo.userId;
            }
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInvitation{"
                    + ", userId='" + userId + '\''
                    + '}';
        }

        public void reset() {
            this.userId = "";
            this.userName = "";
            this.avatarUrl = "";
        }
    }
}
