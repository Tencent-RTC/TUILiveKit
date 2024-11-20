package com.trtc.uikit.livekit.voiceroomcore.state;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.tuikit.common.livedata.LiveListData;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

public class UserState {
    public UserInfo                        selfInfo               = new UserInfo();
    public LiveData<LinkedHashSet<String>> hasAudioStreamUserList = new LiveData<>(new LinkedHashSet<>());
    public LiveListData<UserVolume>        userVolumeList         = new LiveListData<>(new CopyOnWriteArrayList<>());

    public void reset() {
        hasAudioStreamUserList.get().clear();
        userVolumeList.clear();
    }

    public static class UserVolume {
        public UserVolume(String userId) {
            this.userId = userId;
        }

        public String userId;
        public int    volume;

        @Override
        public boolean equals(@Nullable Object obj) {
            if (obj instanceof UserInfo) {
                return this.userId.equals(((UserInfo) obj).userId);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(userId);
        }
    }
}
