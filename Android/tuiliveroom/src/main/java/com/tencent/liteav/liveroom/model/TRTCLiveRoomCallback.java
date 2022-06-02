package com.tencent.liteav.liveroom.model;

import java.util.List;

public class TRTCLiveRoomCallback {
    /**
     * General callbacks
     */
    public interface ActionCallback {
        void onCallback(int code, String msg);
    }

    /**
     * Callback for getting room information
     */
    public interface RoomInfoCallback {
        void onCallback(int code, String msg, List<TRTCLiveRoomDef.TRTCLiveRoomInfo> list);
    }

    /**
     * Callback for getting member information
     */
    public interface UserListCallback {
        void onCallback(int code, String msg, List<TRTCLiveRoomDef.TRTCLiveUserInfo> list);
    }
}
