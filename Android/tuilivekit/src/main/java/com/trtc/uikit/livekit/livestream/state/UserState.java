package com.trtc.uikit.livekit.livestream.state;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

public class UserState {
    public UserInfo                          selfInfo               = new UserInfo();
    public LiveData<LinkedHashSet<UserInfo>> userList               = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>>   hasAudioStreamUserList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>>   hasVideoStreamUserList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>>   speakingUserList       = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<UserInfo>> myFollowingUserList    = new LiveData<>(new LinkedHashSet<>());
    public LiveData<UserInfo>                enterUserInfo          = new LiveData<>();

    public void reset() {
        userList.get().clear();
        hasAudioStreamUserList.get().clear();
        hasVideoStreamUserList.get().clear();
        speakingUserList.get().clear();
        myFollowingUserList.get().clear();
    }

    public void addUserList(Set<UserInfo> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        userList.addAll(list);
    }

    public void addUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        UserInfo info = new UserInfo(userInfo.userId);
        enterUserInfo.set(info);
        userList.add(info);
    }

    public void removeUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        userList.remove(new UserInfo(userInfo.userId));
    }

    public static class UserInfo {
        public String                       userId    = "";
        public LiveData<String>             name      = new LiveData<>("");
        public LiveData<String>             avatarUrl = new LiveData<>("");
        public LiveData<TUIRoomDefine.Role> role      = new LiveData<>(TUIRoomDefine.Role.GENERAL_USER);
        public LiveData<Long>               fansCount = new LiveData<>(0L);

        public UserInfo() {
        }

        public UserInfo(String userId) {
            this.userId = userId;
        }

        public UserInfo(TUIRoomDefine.UserInfo userInfo) {
            updateState(userInfo);
        }

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

        public void updateState(TUIRoomDefine.UserInfo userInfo) {
            this.userId = userInfo.userId;
            this.name.set(userInfo.userName);
            this.avatarUrl.set(userInfo.avatarUrl);
            this.role.set(userInfo.userRole);
        }
    }
}
