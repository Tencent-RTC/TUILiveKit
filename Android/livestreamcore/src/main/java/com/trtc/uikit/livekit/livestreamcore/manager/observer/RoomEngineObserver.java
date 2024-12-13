package com.trtc.uikit.livekit.livestreamcore.manager.observer;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;

import java.util.List;

public class RoomEngineObserver extends TUIRoomObserver {
    private final String mTag = "LiveObserver[" + hashCode() + "]";

    protected LiveStreamManager mVideoLiveManager;

    public RoomEngineObserver(LiveStreamManager manager) {
        mVideoLiveManager = manager;
    }

    @Override
    public void onRoomDismissed(String roomId) {
        Logger.info(mTag + " onRoomDismissed:[roomId" + roomId + "]");
        mVideoLiveManager.getRoomManager().onRoomDismissed(roomId);
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        Logger.info(mTag + " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        Logger.info(mTag + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        mVideoLiveManager.getCoGuestManager().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        Logger.info(mTag + " onRequestReceived:[request:" + new Gson().toJson(request) + "]");
        mVideoLiveManager.getCoGuestManager().onRequestReceived(request);
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        Logger.info(mTag + " onRequestCancelled:[request:" + request + ",operateUser:" + operateUser + "]");
        mVideoLiveManager.getCoGuestManager().onRequestCancelled(request, operateUser);
    }

    @Override
    public void onRequestProcessed(String requestId, String userId) {
        Logger.info(mTag + " onRequestProcessed:[requestId:" + requestId + ",userId:" + userId + "]");
        mVideoLiveManager.getCoGuestManager().onRequestProcessed(requestId, userId);
    }

    @Override
    public void onKickedOffSeat(String userId) {
        Logger.info(mTag + " onKickedOffSeat:[userId:" + userId + "]");
        mVideoLiveManager.getCoGuestManager().onKickedOffSeat(userId);
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        Logger.info(mTag + " onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
        mVideoLiveManager.getUserManager().onUserAudioStateChanged(userId, hasAudio, reason);
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        Logger.info(mTag + " onUserVideoStateChanged:[userId:" + userId + ",hasVideo:" + hasVideo + ",reason:"
                + reason + "]");
        mVideoLiveManager.getUserManager().onUserVideoStateChanged(userId, streamType, hasVideo, reason);
    }

    @Override
    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        Logger.info(mTag + " onUserInfoChanged:[userInfo:" + userInfo + ",modifyFlag:" + modifyFlag + "]");
        mVideoLiveManager.getUserManager().onUserInfoChanged(userInfo, modifyFlag);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(mTag + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(mTag + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
    }

    @Override
    public void onKickedOffLine(String message) {
        Logger.info(mTag + " onKickedOffLine:[message:" + message + "]");
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        Logger.info(mTag + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
    }
}
