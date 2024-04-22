package com.trtc.uikit.livekit.common.core.store.state.operation;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class UserState {
    public UserInfo                          selfInfo               = new UserInfo();
    public UserInfo                          ownerInfo              = new UserInfo();
    public Map<String, UserInfo>             audienceUserMap        = new HashMap<>();
    public LiveData<LinkedHashSet<UserInfo>> audienceList           = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>>   hasAudioStreamUserList = new LiveData<>(new LinkedHashSet<>());
    public LiveData<LinkedHashSet<String>>   hasAudioVolumeUserList = new LiveData<>(new LinkedHashSet<>());

    public void addUserList(Set<UserInfo> list) {
        for (UserInfo userInfo : list) {
            if (userInfo == null) {
                continue;
            }
            if (TextUtils.isEmpty(userInfo.userId)) {
                continue;
            }
            audienceUserMap.put(userInfo.userId, userInfo);
        }
        audienceList.addAll(list);
    }

    public void addUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        if (audienceUserMap.containsKey(userInfo.userId)) {
            return;
        }
        UserInfo userState = new UserInfo(userInfo);
        audienceUserMap.put(userInfo.userId, userState);
        audienceList.add(userState);
    }

    public void removeUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        UserInfo userState = audienceUserMap.get(userInfo.userId);
        if (userState != null) {
            audienceUserMap.remove(userInfo.userId);
            audienceList.remove(userState);
        }
    }

    public static class UserInfo {
        public String                       userId    = "";
        public LiveData<String>             name      = new LiveData<>("");
        public LiveData<String>             avatarUrl = new LiveData<>("");
        public LiveData<TUIRoomDefine.Role> role      = new LiveData<>(TUIRoomDefine.Role.GENERAL_USER);
        public LiveData<Boolean>            isInSeat  = new LiveData<>(false);

        public UserInfo() {
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

        public void updateState(TUIRoomDefine.UserInfo userInfo) {
            this.userId = userInfo.userId;
            this.name.set(userInfo.userName);
            this.avatarUrl.set(userInfo.avatarUrl);
            this.role.set(userInfo.userRole);
        }
    }
}
