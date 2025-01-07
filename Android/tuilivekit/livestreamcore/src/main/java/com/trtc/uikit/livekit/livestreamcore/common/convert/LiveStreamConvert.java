package com.trtc.uikit.livekit.livestreamcore.common.convert;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine;
import com.trtc.uikit.livekit.livestreamcore.view.cdnmodel.VideoViewInfo;

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

    public static UserInfo convertToUserInfo(VideoViewInfo viewInfo) {
        UserInfo userInfo = new UserInfo();
        userInfo.userId = viewInfo.userId;
        userInfo.userName = userInfo.userId;
        userInfo.avatarUrl = "";
        userInfo.hasVideoStream = true;
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

    public static LiveCoreViewDefine.CoHostUser convertToCoHostUser(TUIRoomDefine.UserInfo userInfo,
                                                                    String roomId, boolean hasVideoStream,
                                                                    boolean hasAudioStream) {
        if (userInfo == null) {
            return null;
        }
        TUILiveConnectionManager.ConnectionUser connectionUser = new TUILiveConnectionManager.ConnectionUser();
        connectionUser.roomId = roomId;
        connectionUser.userId = userInfo.userId;
        connectionUser.userName = userInfo.userName;
        connectionUser.avatarUrl = userInfo.avatarUrl;

        LiveCoreViewDefine.CoHostUser coHostUser = new LiveCoreViewDefine.CoHostUser();
        coHostUser.connectionUser = connectionUser;
        coHostUser.hasVideoStream = hasVideoStream;
        coHostUser.hasAudioStream = hasAudioStream;
        return coHostUser;
    }

    public static LiveCoreViewDefine.CoHostUser convertToCoHostUser(TUILiveConnectionManager.ConnectionUser hostUser,
                                                                    boolean hasVideoStream, boolean hasAudioStream) {
        if (hostUser == null) {
            return null;
        }
        LiveCoreViewDefine.CoHostUser coHostUser = new LiveCoreViewDefine.CoHostUser();
        coHostUser.connectionUser = hostUser;
        coHostUser.hasAudioStream = hasAudioStream;
        coHostUser.hasVideoStream = hasVideoStream;
        return coHostUser;
    }

    public static LiveCoreViewDefine.UserInfoModifyFlag convertToUserInfoModifyFlag(TUIRoomDefine.UserInfoModifyFlag flag) {
        switch (flag) {
            case USER_ROLE:
                return LiveCoreViewDefine.UserInfoModifyFlag.USER_ROLE;
            case NAME_CARD:
                return LiveCoreViewDefine.UserInfoModifyFlag.NAME_CARD;
            default:
                return LiveCoreViewDefine.UserInfoModifyFlag.NONE;
        }
    }
}
