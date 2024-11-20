package com.trtc.uikit.livekit.livestreamcore.common.convert;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;

public class LiveStreamConvert {
    public static UserInfo convertToUserInfo(TUIRoomDefine.SeatInfo seatInfo) {
        UserInfo userInfo = new UserInfo();
        userInfo.userId = seatInfo.userId;
        userInfo.userName = seatInfo.userName;
        userInfo.avatarUrl = seatInfo.avatarUrl;
        return userInfo;
    }

    public static UserInfo convertToUserInfo(TUIRoomDefine.Request request) {
        UserInfo userInfo = new UserInfo();
        userInfo.userId = request.userId;
        userInfo.userName = request.userName;
        userInfo.avatarUrl = request.avatarUrl;
        return userInfo;
    }

    public static UserInfo convertToUserInfo(TUILiveConnectionManager.ConnectionUser hostUser) {
        UserInfo userInfo = new UserInfo();
        userInfo.userId = hostUser.userId;
        userInfo.userName = hostUser.userName;
        userInfo.avatarUrl = hostUser.avatarUrl;
        return userInfo;
    }

    public static RoomInfo convertToConnectionUser(TUILiveConnectionManager.ConnectionUser hostUser) {
        if (hostUser == null) {
            return null;
        }
        RoomInfo info = new RoomInfo();
        info.roomId = hostUser.roomId;
        info.ownerId = hostUser.userId;
        info.ownerName = hostUser.userName;
        info.ownerAvatarUrl = hostUser.avatarUrl;
        return info;
    }

    public static TUILiveConnectionManager.ConnectionUser convertToConnectionUser(TUIRoomDefine.UserInfo userInfo, String roomId) {
        if (userInfo == null) {
            return null;
        }
        TUILiveConnectionManager.ConnectionUser connectionUser = new TUILiveConnectionManager.ConnectionUser();
        connectionUser.roomId = roomId;
        connectionUser.userId = userInfo.userId;
        connectionUser.userName = userInfo.userName;
        connectionUser.avatarUrl = userInfo.avatarUrl;
        return connectionUser;
    }
}
