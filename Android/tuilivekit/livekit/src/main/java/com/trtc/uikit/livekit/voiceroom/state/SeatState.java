package com.trtc.uikit.livekit.voiceroom.state;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class SeatState {
    public MutableLiveData<LinkStatus>                     linkStatus             = new MutableLiveData<>(LinkStatus.NONE);
    public MutableLiveData<List<SeatInfo>>                 seatList               =
            new MutableLiveData<>(new CopyOnWriteArrayList<>());
    public MutableLiveData<LinkedHashSet<SeatApplication>> seatApplicationList    =
            new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<Map<String, SeatInvitation>>    sentSeatInvitationMap  = new MutableLiveData<>(new HashMap<>());
    public MutableLiveData<SeatInvitation>                 receivedSeatInvitation = new MutableLiveData<>(new SeatInvitation(""));

    public void reset() {
        linkStatus.setValue(LinkStatus.NONE);
        seatList.getValue().clear();
        seatApplicationList.getValue().clear();
        sentSeatInvitationMap.getValue().clear();
        receivedSeatInvitation.getValue().reset();
    }

    @NonNull
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        for (SeatInfo seatInfo : seatList.getValue()) {
            builder.append("[");
            builder.append(seatInfo.index);
            builder.append(",");
            builder.append(seatInfo.userId.getValue());
            builder.append(",");
            builder.append(seatInfo.isLocked.getValue() ? "1" : "0");
            builder.append(",");
            builder.append(seatInfo.isAudioLocked.getValue() ? "1" : "0");
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
        public int                      index         = 0;
        public MutableLiveData<String>  userId        = new MutableLiveData<>("");
        public MutableLiveData<String>  name          = new MutableLiveData<>("");
        public MutableLiveData<String>  avatarUrl     = new MutableLiveData<>("");
        public MutableLiveData<Boolean> isLocked      = new MutableLiveData<>(false);
        public MutableLiveData<Boolean> isAudioLocked = new MutableLiveData<>(false);
        public MutableLiveData<Boolean> isVideoLocked = new MutableLiveData<>(false);

        public SeatInfo() {
        }

        public SeatInfo(String userId) {
            this.userId.setValue(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + "index=" + index
                    + ", userId=" + userId.getValue()
                    + ", isLocked=" + (isLocked.getValue() ? "1" : "0")
                    + ", isAudioLocked=" + (isAudioLocked.getValue() ? "1" : "0")
                    + ", isVideoLocked=" + (isVideoLocked.getValue() ? "1" : "0")
                    + '}';
        }

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof SeatInfo) {
                return this.userId.getValue().equals(((SeatInfo) obj).userId.getValue());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(userId.getValue());
        }

        public void updateState(TUIRoomDefine.SeatInfo seatInfo) {
            this.index = seatInfo.index;
            this.userId.setValue(seatInfo.userId);
            if (!TextUtils.isEmpty(seatInfo.userName)) {
                this.name.setValue(seatInfo.userName);
            } else {
                this.name.setValue(this.userId.getValue());
            }
            this.avatarUrl.setValue(seatInfo.avatarUrl);
            this.isLocked.setValue(seatInfo.isLocked);
            this.isAudioLocked.setValue(seatInfo.isAudioLocked);
        }

        public void updateState(SeatInfo newSeatInfo) {
            this.index = newSeatInfo.index;
            this.userId.setValue(newSeatInfo.userId.getValue());
            if (!TextUtils.isEmpty(newSeatInfo.name.getValue())) {
                this.name.setValue(newSeatInfo.name.getValue());
            } else {
                this.name.setValue(this.userId.getValue());
            }
            this.avatarUrl.setValue(newSeatInfo.avatarUrl.getValue());
            this.isLocked.setValue(newSeatInfo.isLocked.getValue());
            this.isAudioLocked.setValue(newSeatInfo.isAudioLocked.getValue());
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
