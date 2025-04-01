package com.trtc.uikit.livekit.livestream.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.ConnectionObserver;

import java.util.List;

public class LiveStreamObserver implements ConnectionObserver {
    private final String            mTag = "LiveStreamObserver[" + hashCode() + "]";
    protected     LiveStreamManager mLiveManager;
    private final CoGuestManager    mCoGuestManager;

    public LiveStreamObserver(LiveStreamManager manager) {
        mLiveManager = manager;
        mCoGuestManager = mLiveManager.getCoGuestManager();
    }

    @Override
    public void onConnectedUsersUpdated(List<TUIRoomDefine.UserInfo> userList,
                                        List<UserInfo> joinList,
                                        List<UserInfo> leaveList) {
        LiveStreamLog.info(mTag + " onConnectedUsersUpdated:[userList:" + new Gson().toJson(userList)
                + ",joinList:" + new Gson().toJson(joinList) + ",leaveList:" + new Gson().toJson(leaveList) + "]");
        mCoGuestManager.onSeatListChanged(userList, joinList, leaveList);
    }

    @Override
    public void onUserConnectionRequest(UserInfo inviterUser) {
        LiveStreamLog.info(mTag + " onUserConnectionRequest:[inviterUser:" + new Gson().toJson(inviterUser) + "]");
    }

    @Override
    public void onUserConnectionCancelled(UserInfo inviterUser) {
        LiveStreamLog.info(mTag + " onUserConnectionCancelled:[inviterUser:" + inviterUser + "]");
        mLiveManager.getCoGuestManager().onRequestCancelled(inviterUser);
    }

    @Override
    public void onUserConnectionAccepted(UserInfo liveUser) {
        LiveStreamLog.info(mTag + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
        mLiveManager.getCoGuestManager().onUserConnectionAccepted(liveUser.userId);
    }

    @Override
    public void onUserConnectionRejected(UserInfo liveUser) {
        LiveStreamLog.info(mTag + " onUserConnectionRejected:[liveUser:" + liveUser.userId + "]");
        mLiveManager.getCoGuestManager().onUserConnectionRejected(liveUser.userId);
    }

    @Override
    public void onUserConnectionTimeout(UserInfo liveUser) {
        LiveStreamLog.info(mTag + " onUserConnectionAccepted:[liveUser:" + liveUser.userId + "]");
        mLiveManager.getCoGuestManager().onUserConnectionTimeout(liveUser.userId);
    }

    @Override
    public void onUserConnectionTerminated() {
        LiveStreamLog.info(mTag + " onUserConnectionTerminated:[]");
        mLiveManager.getCoGuestManager().onKickedOffSeat();
    }

    @Override
    public void onUserConnectionExited(UserInfo liveUser) {

    }

    @Override
    public void onConnectedRoomsUpdated(List<ConnectionUser> roomList) {
        LiveStreamLog.info(mTag + " onConnectedRoomsUpdated:[connectedList:" + new Gson().toJson(roomList) + "]");
        mLiveManager.getCoHostManager().onConnectionUserListChanged(roomList);
    }

    @Override
    public void onCrossRoomConnectionRequest(ConnectionUser roomInfo) {
        LiveStreamLog.info(mTag + " onCrossRoomConnectionRequest:[inviter:" + new Gson().toJson(roomInfo) + "]");
        mLiveManager.getCoHostManager().onConnectionRequestReceived(roomInfo);
    }

    @Override
    public void onCrossRoomConnectionCancelled(ConnectionUser roomInfo) {
        LiveStreamLog.info(mTag + " onCrossRoomConnectionCancelled:[inviter:" + new Gson().toJson(roomInfo) + "]");
    }

    @Override
    public void onCrossRoomConnectionAccepted(ConnectionUser roomInfo) {
        LiveStreamLog.info(mTag + " onCrossRoomConnectionAccepted:[invitee:" + new Gson().toJson(roomInfo) + "]");
        mLiveManager.getCoHostManager().onConnectionRequestAccept(roomInfo);
    }

    @Override
    public void onCrossRoomConnectionRejected(ConnectionUser roomInfo) {
        LiveStreamLog.info(mTag + " onConnectionRequestReject:[invitee:" + new Gson().toJson(roomInfo) + "]");
        mLiveManager.getCoHostManager().onConnectionRequestReject(roomInfo);
    }

    @Override
    public void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {
        LiveStreamLog.info(mTag + " onCrossRoomConnectionTimeout:[inviter:" + new Gson().toJson(inviter)
                + ",invitee:" + new Gson().toJson(invitee) + "]");
        mLiveManager.getCoHostManager().onConnectionRequestTimeout(inviter, invitee);
    }

    @Override
    public void onCrossRoomConnectionExited(ConnectionUser roomInfo) {

    }

    @Override
    public void onRoomDismissed(String roomId) {

    }
}
