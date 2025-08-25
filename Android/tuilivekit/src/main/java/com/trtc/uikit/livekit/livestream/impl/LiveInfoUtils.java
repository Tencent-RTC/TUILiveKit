package com.trtc.uikit.livekit.livestream.impl;

import android.os.Bundle;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.ArrayList;
import java.util.Collections;

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

        if (liveInfo.seatMode == null) {
            liveInfo.seatMode = TUIRoomDefine.SeatMode.FREE_TO_TAKE;
        }
        liveBundle.putString("roomId", liveInfo.roomId);
        liveBundle.putString("ownerId", liveInfo.ownerId);
        liveBundle.putString("ownerName", liveInfo.ownerName);
        liveBundle.putString("ownerAvatarUrl", liveInfo.ownerAvatarUrl);
        liveBundle.putString("name", liveInfo.name);
        liveBundle.putBoolean("isMessageDisableForAllUser", liveInfo.isMessageDisableForAllUser);
        liveBundle.putBoolean("isSeatEnabled", liveInfo.isSeatEnabled);
        liveBundle.putInt("seatMode", liveInfo.seatMode.getValue());
        liveBundle.putInt("maxSeatCount", liveInfo.maxSeatCount);
        liveBundle.putLong("createTime", liveInfo.createTime);
        return liveBundle;
    }

    public static TUILiveListManager.LiveInfo convertBundleToLiveInfo(Bundle liveBundle) {
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.roomId = liveBundle.getString("roomId", "");
        liveInfo.ownerId = liveBundle.getString("ownerId", "");
        liveInfo.ownerName = liveBundle.getString("ownerName", "");
        liveInfo.ownerAvatarUrl = liveBundle.getString("ownerAvatarUrl", "");
        liveInfo.name = liveBundle.getString("name", "");
        liveInfo.isMessageDisableForAllUser = liveBundle.getBoolean("isMessageDisableForAllUser", false);
        liveInfo.isSeatEnabled = liveBundle.getBoolean("isSeatEnabled", false);
        liveInfo.seatMode = TUIRoomDefine.SeatMode.fromInt(liveBundle.getInt("seatMode", TUIRoomDefine.SeatMode.FREE_TO_TAKE.getValue()));
        liveInfo.maxSeatCount = liveBundle.getInt("maxSeatCount", 0);
        liveInfo.createTime = liveBundle.getLong("createTime", 0);
        liveInfo.coverUrl = liveBundle.getString("coverUrl", "");
        liveInfo.backgroundUrl = liveBundle.getString("backgroundUrl", "");
        liveInfo.categoryList = liveBundle.getIntegerArrayList("categoryList");
        if (liveInfo.categoryList == null) {
            liveInfo.categoryList = Collections.EMPTY_LIST;
        }
        liveInfo.isPublicVisible = liveBundle.getBoolean("isPublicVisible", false);
        liveInfo.activityStatus = liveBundle.getInt("activityStatus", 0);
        liveInfo.viewCount = liveBundle.getInt("viewCount", 0);
        return liveInfo;
    }
}
