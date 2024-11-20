package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoHostManager;

import java.util.List;

public class LiveConnectionManagerObserver extends TUILiveConnectionManager.Observer {
    private final String            mTag = "LiveConnectionManagerObserver[" + hashCode() + "]";
    protected     LiveStreamManager mVideoLiveManager;
    private final CoHostManager     mCoHostManager;

    public LiveConnectionManagerObserver(LiveStreamManager manager) {
        mVideoLiveManager = manager;
        mCoHostManager = mVideoLiveManager.getCoHostManager();
    }

    public void onConnectionUserListChanged(List<TUILiveConnectionManager.ConnectionUser> connectedList,
                                            List<TUILiveConnectionManager.ConnectionUser> joinedList,
                                            List<TUILiveConnectionManager.ConnectionUser> leavedList) {
        Logger.info(mTag + " onConnectionUserListChanged:[connectedList:" + new Gson().toJson(connectedList)
                + ",joinedList:" + new Gson().toJson(joinedList) + ",leavedList:" + new Gson().toJson(leavedList)
                + "]");
        mCoHostManager.onConnectionUserListChanged(connectedList, joinedList, leavedList);
    }

    public void onConnectionRequestReceived(TUILiveConnectionManager.ConnectionUser inviter,
                                            List<TUILiveConnectionManager.ConnectionUser> inviteeList,
                                            String extensionInfo) {
        Logger.info(mTag + " onConnectionRequestReceived:[inviter:" + new Gson().toJson(inviter)
                + ",inviteeList:" + new Gson().toJson(inviteeList)
                + ",extensionInfo:" + new Gson().toJson(extensionInfo) + "]");
        mCoHostManager.onConnectionRequestReceived(inviter, inviteeList, extensionInfo);
    }

    public void onConnectionRequestCancelled(TUILiveConnectionManager.ConnectionUser inviter) {
        Logger.info(mTag + " onConnectionRequestCancelled:[inviter:" + new Gson().toJson(inviter) + "]");
        mCoHostManager.onConnectionRequestCanceled(inviter);
    }

    public void onConnectionRequestAccept(TUILiveConnectionManager.ConnectionUser invitee) {
        Logger.info(mTag + " onConnectionRequestAccept:[invitee:" + new Gson().toJson(invitee) + "]");
        mCoHostManager.onConnectionRequestAccept(invitee);
    }

    public void onConnectionRequestReject(TUILiveConnectionManager.ConnectionUser invitee) {
        Logger.info(mTag + " onConnectionRequestReject:[invitee:" + new Gson().toJson(invitee) + "]");
        mCoHostManager.onConnectionRequestReject(invitee);
    }

    public void onConnectionRequestTimeout(TUILiveConnectionManager.ConnectionUser inviter,
                                           TUILiveConnectionManager.ConnectionUser invitee) {
        Logger.info(mTag + " onConnectionRequestTimeout:[inviter:" + new Gson().toJson(inviter)
                + ",invitee:" + new Gson().toJson(invitee) + "]");
        mCoHostManager.onConnectionRequestTimeout(inviter, invitee);
    }

}
