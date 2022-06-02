package com.tencent.liteav.liveroom;

public interface TUILiveRoomListener {
    /**
     * Create room callback
     * @param code 0: success. else: fail
     * @param message result message
     */
    void onRoomCreate(int code, String message);

    /**
     * Enter room callback
     * @param code 0: success. else: fail
     * @param message result message
     */
    void onRoomEnter(int code, String message);
}
