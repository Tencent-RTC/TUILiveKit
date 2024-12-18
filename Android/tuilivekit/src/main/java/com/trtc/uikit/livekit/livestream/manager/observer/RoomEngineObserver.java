package com.trtc.uikit.livekit.livestream.manager.observer;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private final String mTag = "LiveObserver[" + hashCode() + "]";

    protected LiveStreamManager mLiveManager;

    public RoomEngineObserver(LiveStreamManager manager) {
        mLiveManager = manager;
    }

    @Override
    public void onRoomDismissed(String roomId) {
        LiveStreamLog.info(mTag + " onRoomDismissed:[roomId" + roomId + "]");
        mLiveManager.getRoomManager().onLiveEnd(roomId);
    }


    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LiveStreamLog.info(mTag + " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
        mLiveManager.getRoomManager().onRoomUserCountChanged(roomId, userCount);
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LiveStreamLog.info(mTag + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        LiveStreamLog.info(mTag + " onRequestReceived:[request:" + new Gson().toJson(request) + "]");
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        LiveStreamLog.info(mTag + " onRequestCancelled:[request:" + request + ",operateUser:" + operateUser + "]");
    }

    @Override
    public void onRequestProcessed(String requestId, String userId) {

    }

    @Override
    public void onKickedOffSeat(String userId) {

    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        LiveStreamLog.info(mTag + " onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
        mLiveManager.getUserManager().onUserAudioStateChanged(userId, hasAudio, reason);
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        LiveStreamLog.info(mTag + " onUserVideoStateChanged:[userId:" + userId + ",hasVideo:" + hasVideo + ",reason:"
                + reason + "]");
        mLiveManager.getUserManager().onUserVideoStateChanged(userId, streamType, hasVideo, reason);
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        mLiveManager.getUserManager().onUserVoiceVolumeChanged(volumeMap);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveStreamLog.info(mTag + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveManager.getUserManager().onRemoteUserEnterRoom(roomId, userInfo);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveStreamLog.info(mTag + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveManager.getUserManager().onRemoteUserLeaveRoom(roomId, userInfo);
    }

    @Override
    public void onKickedOffLine(String message) {
        LiveStreamLog.info(mTag + " onKickedOffLine:[message:" + message + "]");
        ErrorHandler.handleMessage(message);
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, null);
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LiveStreamLog.info(mTag + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ErrorHandler.handleMessage(message);
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", roomId);
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        }
    }

    @Override
    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        LiveStreamLog.info(mTag + "onUserInfoChanged:[userInfo:" + new Gson().toJson(userInfo)
                + ", modifyFlag:" + new Gson().toJson(modifyFlag));
        mLiveManager.getUserManager().onUserInfoChanged(userInfo, modifyFlag);
    }
}
