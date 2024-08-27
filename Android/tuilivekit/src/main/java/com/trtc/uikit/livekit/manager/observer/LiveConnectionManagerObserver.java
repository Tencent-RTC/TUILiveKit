package com.trtc.uikit.livekit.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.ConnectionController;

import java.util.List;

public class LiveConnectionManagerObserver extends TUILiveConnectionManager.Observer {
    private final String               mTag = "LiveConnectionManagerObserver[" + hashCode() + "]";
    protected     LiveController       mLiveController;
    private final ConnectionController mConnectionController;

    public LiveConnectionManagerObserver(LiveController liveController) {
        mLiveController = liveController;
        mConnectionController = mLiveController.getConnectionController();
    }

    public void onConnectionUserListChanged(List<TUILiveConnectionManager.ConnectionUser> connectedList,
                                            List<TUILiveConnectionManager.ConnectionUser> joinedList,
                                            List<TUILiveConnectionManager.ConnectionUser> leavedList) {
        LiveKitLog.info(mTag + " onConnectionUserListChanged:[connectedList:" + new Gson().toJson(connectedList)
            + ",joinedList:" + new Gson().toJson(joinedList) + ",leavedList:" + new Gson().toJson(leavedList) + "]");
        mConnectionController.onConnectionUserListChanged(connectedList, joinedList, leavedList);
    }

    public void onConnectionRequestReceived(TUILiveConnectionManager.ConnectionUser inviter,
                                            List<TUILiveConnectionManager.ConnectionUser> inviteeList,
                                            String extensionInfo) {
        LiveKitLog.info(mTag + " onConnectionRequestReceived:[inviter:" + new Gson().toJson(inviter)
                + ",inviteeList:" + new Gson().toJson(inviteeList)
                + ",extensionInfo:" + new Gson().toJson(extensionInfo) + "]");
        mConnectionController.onConnectionRequestReceived(inviter, inviteeList, extensionInfo);
    }

    public void onConnectionRequestCancelled(TUILiveConnectionManager.ConnectionUser inviter) {
        LiveKitLog.info(mTag + " onConnectionRequestCancelled:[inviter:" + new Gson().toJson(inviter) + "]");
    }

    public void onConnectionRequestAccept(TUILiveConnectionManager.ConnectionUser invitee) {
        LiveKitLog.info(mTag + " onConnectionRequestAccept:[invitee:" + new Gson().toJson(invitee) + "]");
        mConnectionController.onConnectionRequestAccept(invitee);
    }

    public void onConnectionRequestReject(TUILiveConnectionManager.ConnectionUser invitee) {
        LiveKitLog.info(mTag + " onConnectionRequestReject:[invitee:" + new Gson().toJson(invitee) + "]");
        mConnectionController.onConnectionRequestReject(invitee);
    }

    public void onConnectionRequestTimeout(TUILiveConnectionManager.ConnectionUser inviter,
                                           TUILiveConnectionManager.ConnectionUser invitee) {
        LiveKitLog.info(mTag + " onConnectionRequestTimeout:[inviter:" + new Gson().toJson(inviter)
                + ",invitee:" + new Gson().toJson(invitee) + "]");
        mConnectionController.onConnectionRequestTimeout(inviter,invitee);
    }

}
