package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.ConnectionObserver;

import java.lang.ref.WeakReference;
import java.util.List;

public class LiveStreamObserver implements ConnectionObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("LiveStreamObserver");

    protected WeakReference<AudienceManager> mLiveManager;

    public LiveStreamObserver(AudienceManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onConnectedUsersUpdated(List<UserInfo> userList, List<UserInfo> joinList,
                                        List<UserInfo> leaveList) {
        LOGGER.info(hashCode() + " onConnectedUsersUpdated:[userList:" + new Gson().toJson(userList) + ",joinList:" + new Gson().toJson(joinList) + ",leaveList:" + new Gson().toJson(leaveList) + "]");
    }

    @Override
    public void onUserConnectionRequest(UserInfo inviterUser) {
        LOGGER.info(hashCode() + " onUserConnectionRequest:[inviterUser:" + new Gson().toJson(inviterUser) + "]");
    }

    @Override
    public void onUserConnectionCancelled(UserInfo inviterUser) {
        LOGGER.info(hashCode() + " onUserConnectionCancelled:[inviterUser:" + inviterUser + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onRequestCancelled(inviterUser);
        }
    }

    @Override
    public void onUserConnectionAccepted(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onUserConnectionAccepted(liveUser.userId);
        }
    }

    @Override
    public void onUserConnectionRejected(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionRejected:[liveUser:" + liveUser.userId + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onUserConnectionRejected(liveUser.userId);
        }
    }

    @Override
    public void onUserConnectionTimeout(UserInfo liveUser) {
        LOGGER.info(hashCode() + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onUserConnectionTimeout(liveUser.userId);
        }
    }

    @Override
    public void onUserConnectionTerminated() {
        LOGGER.info(hashCode() + " onUserConnectionTerminated:[]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onKickedOffSeat();
        }
    }

    @Override
    public void onUserConnectionExited(UserInfo liveUser) {

    }

    @Override
    public void onConnectedRoomsUpdated(List<ConnectionUser> roomList) {

    }

    @Override
    public void onCrossRoomConnectionRequest(ConnectionUser roomInfo) {

    }

    @Override
    public void onCrossRoomConnectionCancelled(ConnectionUser roomInfo) {

    }

    @Override
    public void onCrossRoomConnectionAccepted(ConnectionUser roomInfo) {

    }

    @Override
    public void onCrossRoomConnectionRejected(ConnectionUser roomInfo) {

    }

    @Override
    public void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {

    }

    @Override
    public void onCrossRoomConnectionExited(ConnectionUser roomInfo) {

    }

    @Override
    public void onRoomDismissed(String roomId) {

    }
}
