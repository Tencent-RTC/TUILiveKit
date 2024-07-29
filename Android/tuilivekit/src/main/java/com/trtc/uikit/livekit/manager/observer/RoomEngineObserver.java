package com.trtc.uikit.livekit.manager.observer;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.KickedOutOfRoomReason.BY_LOGGED_ON_OTHER_DEVICE;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private final String mTag = "LiveObserver[" + hashCode() + "]";

    protected LiveController mLiveController;

    public RoomEngineObserver(LiveController liveController) {
        mLiveController = liveController;
    }

    @Override
    public void onRoomDismissed(String roomId) {
        LiveKitLog.info(mTag + " onRoomDismissed:[roomId" + roomId + "]");
        mLiveController.getViewController().onLiveEnd(roomId);
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LiveKitLog.info(mTag + " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
        mLiveController.getRoomController().onRoomUserCountChanged(roomId, userCount);
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LiveKitLog.info(mTag + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        mLiveController.getSeatController().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        LiveKitLog.info(mTag + " onRequestReceived:[request:" + new Gson().toJson(request) + "]");
        mLiveController.getSeatController().onRequestReceived(request);
    }

    @Override
    public void onRequestCancelled(String requestId, String userId) {
        LiveKitLog.info(mTag + " onRequestCancelled:[requestId:" + requestId + ",userId:" + userId + "]");
        mLiveController.getSeatController().onRequestCancelled(requestId, userId);
    }

    @Override
    public void onRequestProcessed(String requestId, String userId) {
        LiveKitLog.info(mTag + " onRequestProcessed:[requestId:" + requestId + ",userId:" + userId + "]");
        mLiveController.getSeatController().onRequestProcessed(requestId, userId);
    }

    @Override
    public void onKickedOffSeat(String userId) {
        LiveKitLog.info(mTag + " onKickedOffSeat:[userId:" + userId + "]");
        mLiveController.getSeatController().onKickedOffSeat(userId);
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info(mTag + " onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
        mLiveController.getUserController().onUserAudioStateChanged(userId, hasAudio, reason);
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info(mTag + " onUserVideoStateChanged:[userId:" + userId + ",hasVideo:" + hasVideo + ",reason:"
                + reason + "]");
        mLiveController.getUserController().onUserVideoStateChanged(userId, streamType, hasVideo, reason);
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        mLiveController.getUserController().onUserVoiceVolumeChanged(volumeMap);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info(mTag + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveController.getUserController().onRemoteUserEnterRoom(roomId, userInfo);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info(mTag + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveController.getUserController().onRemoteUserLeaveRoom(roomId, userInfo);
    }

    @Override
    public void onKickedOffLine(String message) {
        LiveKitLog.info(mTag + " onKickedOffLine:[message:" + message + "]");
        ErrorHandler.handleMessage(message);
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, null);
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LiveKitLog.info(mTag + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        if (reason != null && BY_LOGGED_ON_OTHER_DEVICE != reason) {
            ErrorHandler.handleMessage(message);
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", roomId);
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        }
    }
}
