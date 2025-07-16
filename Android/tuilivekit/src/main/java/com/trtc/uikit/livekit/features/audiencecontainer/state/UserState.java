package com.trtc.uikit.livekit.features.audiencecontainer.state;

import android.text.TextUtils;

import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

public class UserState {
    public MutableLiveData<Set<UserInfo>> userList          = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<Set<String>>   speakingUserList  = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<Set<String>>   followingUserList = new MutableLiveData<>(new LinkedHashSet<>());

    public void reset() {
        userList.getValue().clear();
        speakingUserList.getValue().clear();
        followingUserList.getValue().clear();
    }

    public void addUser(UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        userList.getValue().add(userInfo);
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
        public String                              userId            = "";
        public MutableLiveData<String>             name              = new MutableLiveData<>("");
        public MutableLiveData<String>             avatarUrl         = new MutableLiveData<>("");
        public MutableLiveData<TUIRoomDefine.Role> role              =
                new MutableLiveData<>(TUIRoomDefine.Role.GENERAL_USER);
        public MutableLiveData<Boolean>            isMessageDisabled = new MutableLiveData<>(false);

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
            this.isMessageDisabled.setValue(userInfo.isMessageDisabled);
        }
    }
}
