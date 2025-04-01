package com.trtc.uikit.livekit.livestream.state;


import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.NONE;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

public class CoGuestState {
    public MutableLiveData<List<SeatInfo>> connectedUserList =
            new MutableLiveData<>(new CopyOnWriteArrayList<>());
    public MutableLiveData<CoGuestStatus>  coGuestStatus     = new MutableLiveData<>(NONE);
    public MutableLiveData<Set<String>>    lockAudioUserList = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<Set<String>>    lockVideoUserList = new MutableLiveData<>(new LinkedHashSet<>());

    public void reset() {
        connectedUserList.getValue().clear();
        coGuestStatus.setValue(NONE);
    }

    public static class SeatInfo {
        public MutableLiveData<String> userId    = new MutableLiveData<>("");
        public MutableLiveData<String> name      = new MutableLiveData<>("");
        public MutableLiveData<String> avatarUrl = new MutableLiveData<>("");

        public SeatInfo() {
        }

        public SeatInfo(String userId) {
            this.userId.setValue(userId);
        }

        @NonNull
        @Override
        public String toString() {
            return "SeatInfo{"
                    + ", userId=" + userId.getValue()
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

        public void updateState(TUIRoomDefine.UserInfo seatInfo) {
            this.userId.setValue(seatInfo.userId);
            if (!TextUtils.isEmpty(seatInfo.userName)) {
                this.name.setValue(seatInfo.userName);
            } else {
                this.name.setValue(this.userId.getValue());
            }
            this.avatarUrl.setValue(seatInfo.avatarUrl);
        }
    }

    public enum CoGuestStatus {
        NONE,
        APPLYING,
        LINKING
    }

}
