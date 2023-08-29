package com.tencent.liteav.liveroom.model;

public interface TRTCLiveRoomDelegate {

    /**
     * Component error message, which must be listened for and handled
     */
    void onError(int code, String message);

    /**
     * Component warning message
     */
    void onWarning(int code, String message);

    /**
     * Component log message
     */
    void onDebugLog(String message);

    /**
     * Notification of room information change
     * <p>
     * Callback for room information change.
     * This callback is usually used to notify users of room status change in co-anchoring
     * and cross-room communication scenarios.
     *
     * @param roomInfo Room information
     */
    void onRoomInfoChange(TRTCLiveRoomDef.TRTCLiveRoomInfo roomInfo);

    /**
     * Callback for room termination, which will be received by audience members after the anchor calls `destroyRoom`
     */
    void onRoomDestroy(String roomId);

    /**
     * The anchor entered the room. Call `startPlay()` to start playing back the user's video
     */
    void onAnchorEnter(String userId);

    /**
     * The anchor exited the room. Call `stopPlay()` to stop playing back the user's video
     */
    void onAnchorExit(String userId);

    /**
     * An audience member entered the room
     */
    void onAudienceEnter(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo);

    /**
     * An audience member exited the room
     */
    void onAudienceExit(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo);

    /**
     * an user video stream available or unAvailable
     *
     * @param userId    userId
     * @param available video stream available or unAvailable
     */
    void onUserVideoAvailable(String userId, boolean available);

    /**
     * The anchor received a co-anchoring request
     *
     * @param userInfo Co-anchoring user
     * @param reason   Reason
     * @param timeOut  Timeout period for response from the anchor.
     *                 If the anchor does not respond to the request within the period,
     *                 it will be discarded automatically.
     */
    void onRequestJoinAnchor(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo, String reason, int timeOut);

    /**
     * An audience member canceled a co-anchoring request
     */
    void onCancelJoinAnchor();

    /**
     * An audience member was removed from co-anchoring
     */
    void onKickoutJoinAnchor();

    /**
     * A cross-room communication request was received
     * <p>
     * If the anchor accepts a cross-room communication request from an anchor in another room,
     * he or she should wait for the `onAnchorEnter` callback from `TRTCLiveRoomDelegate`
     * and then call `startPlay()` to play the other anchor's video.
     *
     * @param userInfo
     * @param timeout  Timeout period for response from the anchor.
     *                 If the anchor does not respond to the request within the period,
     *                 it will be discarded automatically.
     */
    void onRequestRoomPK(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo, int timeout);

    /**
     * The anchor received a request to cancel cross-room communication from another anchor
     */
    void onCancelRoomPK();

    /**
     * The anchor ended cross-room communication.
     */
    void onQuitRoomPK();

    /**
     * A text chat message was received.
     */
    void onRecvRoomTextMsg(String message, TRTCLiveRoomDef.TRTCLiveUserInfo userInfo);

    /**
     * A custom message was received.
     */
    void onRecvRoomCustomMsg(String cmd, String message, TRTCLiveRoomDef.TRTCLiveUserInfo userInfo);

    /**
     * The co-anchoring response timed out
     */
    void onAudienceRequestJoinAnchorTimeout(String userId);

    /**
     * The cross-room communication response timed out
     */
    void onAnchorRequestRoomPKTimeout(String userId);

}
