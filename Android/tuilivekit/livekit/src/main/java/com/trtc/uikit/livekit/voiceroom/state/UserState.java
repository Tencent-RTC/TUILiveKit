package com.trtc.uikit.livekit.voiceroom.state;

import android.text.TextUtils;

import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

public class UserState {
    public UserInfo                       selfInfo            = new UserInfo();
    public MutableLiveData<Set<UserInfo>> userList            = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<Set<UserInfo>> myFollowingUserList = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<UserInfo>      enterUserInfo       = new MutableLiveData<>();

    public void reset() {
        userList.getValue().clear();
        myFollowingUserList.getValue().clear();
    }

    public void addUserList(Set<UserInfo> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        userList.getValue().addAll(list);
        userList.setValue(userList.getValue());
    }

    public void addUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        UserInfo info = new UserInfo(userInfo);
        enterUserInfo.setValue(info);
        userList.getValue().add(new UserInfo(userInfo));
        userList.setValue(userList.getValue());
    }

    public void removeUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        userList.getValue().remove(new UserInfo(userInfo.userId));
        userList.setValue(userList.getValue());
    }

    public static class UserInfo {
        public String                              userId    = "";
        public MutableLiveData<String>             name      = new MutableLiveData<>("");
        public MutableLiveData<String>             avatarUrl = new MutableLiveData<>("");
        public MutableLiveData<TUIRoomDefine.Role> role      = new MutableLiveData<>(TUIRoomDefine.Role.GENERAL_USER);
        public MutableLiveData<Long>               fansCount = new MutableLiveData<>(0L);

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
            this.name.setValue(userInfo.userName);
            this.avatarUrl.setValue(userInfo.avatarUrl);
            this.role.setValue(userInfo.userRole);
        }
    }
}
