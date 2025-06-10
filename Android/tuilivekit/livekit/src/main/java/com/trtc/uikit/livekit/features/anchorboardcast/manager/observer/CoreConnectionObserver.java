package com.trtc.uikit.livekit.features.anchorboardcast.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.ConnectionObserver;

import java.util.List;

public class CoreConnectionObserver implements ConnectionObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("CoreConnectionObserver");

    protected AnchorManager mManager;

    public CoreConnectionObserver(AnchorManager manager) {
        mManager = manager;
    }

    @Override
    public void onConnectedUsersUpdated(List<UserInfo> userList,
                                        List<UserInfo> joinList,
                                        List<UserInfo> leaveList) {
        LOGGER.info(hashCode() + " onConnectedUsersUpdated:[userList:" + new Gson().toJson(userList)
                + ",joinList:" + new Gson().toJson(joinList) + ",leaveList:" + new Gson().toJson(leaveList) + "]");
    }

    @Override
    public void onUserConnectionRequest(UserInfo inviterUser) {
        LOGGER.info(hashCode() + " onUserConnectionRequest:[inviterUser:" + new Gson().toJson(inviterUser) + "]");
    }

    @Override
    public void onUserConnectionCancelled(UserInfo inviterUser) {
        LOGGER.info(hashCode() + " onUserConnectionCancelled:[inviterUser:" + inviterUser + "]");
    }

    @Override
    public void onUserConnectionAccepted(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
    }

    @Override
    public void onUserConnectionRejected(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionRejected:[liveUser:" + liveUser.userId + "]");
        mManager.onUserConnectionRejected(liveUser.userId);
    }

    @Override
    public void onUserConnectionTimeout(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
        mManager.onUserConnectionTimeout(liveUser.userId);
    }

    @Override
    public void onUserConnectionTerminated() {
        LOGGER.info(hashCode() + " onUserConnectionTerminated:[]");
        mManager.onKickedOffSeat();
    }

    @Override
    public void onUserConnectionExited(UserInfo liveUser) {

    }

    @Override
    public void onConnectedRoomsUpdated(List<ConnectionUser> roomList) {
        LOGGER.info(hashCode() + " onConnectedRoomsUpdated:[connectedList:" + new Gson().toJson(roomList) + "]");
        mManager.onConnectionUserListChanged(roomList);
    }

    @Override
    public void onCrossRoomConnectionRequest(ConnectionUser roomInfo) {
        LOGGER.info(hashCode() + " onCrossRoomConnectionRequest:[inviter:" + new Gson().toJson(roomInfo) + "]");
    }

    @Override
    public void onCrossRoomConnectionCancelled(ConnectionUser roomInfo) {
        LOGGER.info(hashCode() + " onCrossRoomConnectionCancelled:[inviter:" + new Gson().toJson(roomInfo) + "]");
    }

    @Override
    public void onCrossRoomConnectionAccepted(ConnectionUser roomInfo) {
        LOGGER.info(hashCode() + " onCrossRoomConnectionAccepted:[invitee:" + new Gson().toJson(roomInfo) + "]");
    }

    @Override
    public void onCrossRoomConnectionRejected(ConnectionUser roomInfo) {
        LOGGER.info(hashCode() + " onConnectionRequestReject:[invitee:" + new Gson().toJson(roomInfo) + "]");
        mManager.onConnectionRequestReject(roomInfo);
    }

    @Override
    public void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {
        LOGGER.info(hashCode() + " onCrossRoomConnectionTimeout:[inviter:" + new Gson().toJson(inviter)
                + ",invitee:" + new Gson().toJson(invitee) + "]");
    }

    @Override
    public void onCrossRoomConnectionExited(ConnectionUser roomInfo) {
    }

    @Override
    public void onRoomDismissed(String roomId) {
    }
}
