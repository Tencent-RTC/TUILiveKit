package com.trtc.uikit.livekit;

import android.os.Bundle;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.ArrayList;

public class LiveInfoUtils {

    public static Bundle convertLiveInfoToBundle(TUILiveListManager.LiveInfo liveInfo) {
        Bundle liveBundle = new Bundle();
        liveBundle.putString("coverUrl", liveInfo.coverUrl);
        liveBundle.putString("backgroundUrl", liveInfo.backgroundUrl);
        if (liveInfo.categoryList == null) {
            liveBundle.putIntegerArrayList("categoryList", new ArrayList<>());
        } else {
            liveBundle.putIntegerArrayList("categoryList", new ArrayList<>(liveInfo.categoryList));
        }
        liveBundle.putBoolean("isPublicVisible", liveInfo.isPublicVisible);
        liveBundle.putInt("activityStatus", liveInfo.activityStatus);
        liveBundle.putInt("viewCount", liveInfo.viewCount);

        Bundle roomBundle = new Bundle();
        TUIRoomDefine.RoomInfo roomInfo = liveInfo.roomInfo;
        if (roomInfo == null) {
            roomInfo = new TUIRoomDefine.RoomInfo();
        }
        if (roomInfo.seatMode == null) {
            roomInfo.seatMode = TUIRoomDefine.SeatMode.FREE_TO_TAKE;
        }
        if (roomInfo.roomType == null) {
            roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        }
        roomBundle.putString("roomId", roomInfo.roomId);
        roomBundle.putString("ownerId", roomInfo.ownerId);
        roomBundle.putString("ownerName", roomInfo.ownerName);
        roomBundle.putString("ownerAvatarUrl", roomInfo.ownerAvatarUrl);
        roomBundle.putInt("roomType", roomInfo.roomType.getValue());
        roomBundle.putString("name", roomInfo.name);
        roomBundle.putBoolean("isCameraDisableForAllUser", roomInfo.isCameraDisableForAllUser);
        roomBundle.putBoolean("isMicrophoneDisableForAllUser", roomInfo.isMicrophoneDisableForAllUser);
        roomBundle.putBoolean("isScreenShareDisableForAllUser", roomInfo.isScreenShareDisableForAllUser);
        roomBundle.putBoolean("isMessageDisableForAllUser", roomInfo.isMessageDisableForAllUser);
        roomBundle.putBoolean("isSeatEnabled", roomInfo.isSeatEnabled);
        roomBundle.putInt("seatMode", roomInfo.seatMode.getValue());
        roomBundle.putInt("maxSeatCount", roomInfo.maxSeatCount);
        roomBundle.putLong("createTime", roomInfo.createTime);
        roomBundle.putInt("memberCount", roomInfo.memberCount);
        roomBundle.putString("password", roomInfo.password);

        liveBundle.putBundle("roomInfo", roomBundle);
        return liveBundle;
    }

    public static TUILiveListManager.LiveInfo convertBundleToLiveInfo(Bundle liveBundle) {
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.coverUrl = liveBundle.getString("coverUrl");
        liveInfo.backgroundUrl = liveBundle.getString("backgroundUrl");
        liveInfo.categoryList = liveBundle.getIntegerArrayList("categoryList");
        liveInfo.isPublicVisible = liveBundle.getBoolean("isPublicVisible", false);
        liveInfo.activityStatus = liveBundle.getInt("activityStatus", 0);
        liveInfo.viewCount = liveBundle.getInt("viewCount", 0);

        TUIRoomDefine.RoomInfo roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo = roomInfo;

        Bundle roomBundle = liveBundle.getBundle("roomInfo");
        if (roomBundle != null) {
            roomInfo.roomId = roomBundle.getString("roomId");
            roomInfo.ownerId = roomBundle.getString("ownerId");
            roomInfo.ownerName = roomBundle.getString("ownerName");
            roomInfo.ownerAvatarUrl = roomBundle.getString("ownerAvatarUrl");
            roomInfo.roomType = TUIRoomDefine.RoomType.fromInt(roomBundle.getInt("roomType", 0));
            roomInfo.name = roomBundle.getString("name");
            roomInfo.isCameraDisableForAllUser = roomBundle.getBoolean("isCameraDisableForAllUser", false);
            roomInfo.isMicrophoneDisableForAllUser = roomBundle.getBoolean("isMicrophoneDisableForAllUser", false);
            roomInfo.isScreenShareDisableForAllUser = roomBundle.getBoolean("isScreenShareDisableForAllUser", false);
            roomInfo.isMessageDisableForAllUser = roomBundle.getBoolean("isMessageDisableForAllUser", false);
            roomInfo.isSeatEnabled = roomBundle.getBoolean("isSeatEnabled", false);
            roomInfo.seatMode = TUIRoomDefine.SeatMode.fromInt(roomBundle.getInt("seatMode", 0));
            roomInfo.maxSeatCount = roomBundle.getInt("maxSeatCount", 0);
            roomInfo.createTime = roomBundle.getLong("createTime", 0);
            roomInfo.memberCount = roomBundle.getInt("memberCount", 0);
            roomInfo.password = roomBundle.getString("password");
        }
        return liveInfo;
    }
}
