package com.trtc.uikit.livekit.liveroom.data;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.RoleType;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserInteractionStatus;

import java.util.Objects;

public class UserInfo implements Comparable<UserInfo> {

    public String                          userId     = "";
    public LiveData<String>                name       = new LiveData<>("");
    public LiveData<String>                avatarUrl  = new LiveData<>("");
    public LiveData<RoleType>              role       = new LiveData<>(RoleType.AUDIENCE);
    public LiveData<UserInteractionStatus> status     = new LiveData<>(UserInteractionStatus.NONE);
    public AudioInfo                       audioInfo  = new AudioInfo();
    public VideoInfo                       videoInfo  = new VideoInfo();
    public BeautyInfo                      beautyInfo = new BeautyInfo();
    public String                          requestId  = "";

    public UserInfo() {
    }

    public UserInfo(String userId) {
        this.userId = userId;
    }

    public UserInfo(TUIRoomDefine.UserInfo userInfo) {
        update(userInfo);
    }

    public void update(TUIRoomDefine.UserInfo userInfo) {
        this.userId = userInfo.userId;
        this.name.set(userInfo.userName);
        this.avatarUrl.set(userInfo.avatarUrl);
        this.role.set((userInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) ? RoleType.ANCHOR : RoleType.AUDIENCE);
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

    @NonNull
    @Override
    public String toString() {
        return ("UserInfo {"
                + "userId='" + userId
                + ", name='" + name
                + ", avatarUrl='" + avatarUrl
                + ", role='" + role
                + ", status=" + status
                + ", audioInfo=" + audioInfo.toString()
                + ", videoInfo=" + videoInfo.toString()
                + ", beautyInfo" + beautyInfo.toString()
                + ", requestId=" + requestId
                + '}');
    }

    @Override
    public int compareTo(UserInfo userInfo) {
        if (this.userId.equals(userInfo.userId)) {
            return 0;
        } else {
            return 1;
        }
    }
}
