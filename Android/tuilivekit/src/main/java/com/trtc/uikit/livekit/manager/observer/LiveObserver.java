package com.trtc.uikit.livekit.manager.observer;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;

import java.util.List;
import java.util.Map;

public class LiveObserver extends TUIRoomObserver {
    private static final String TAG = "LiveObserver";

    protected LiveController mLiveController;

    public LiveObserver(LiveController liveController) {
        mLiveController = liveController;
    }

    @Override
    public void onRoomDismissed(String roomId) {
        LiveKitLog.info(TAG + " onRoomDismissed : " + roomId);
        mLiveController.getViewController().onLiveEnd(roomId);
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LiveKitLog.info(TAG + " onRoomUserCountChanged : " + userCount);
        mLiveController.getRoomController().onRoomUserCountChanged(roomId, userCount);
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LiveKitLog.info(TAG + " onSeatInfoChanged:" + mLiveController.getSeatState().toString());
        mLiveController.getSeatController().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        LiveKitLog.info(TAG + " onRequestReceived takeSeat,id:" + request.requestId + ",userId:" + request.userId);
        mLiveController.getSeatController().onRequestReceived(request);
    }

    @Override
    public void onRequestCancelled(String requestId, String userId) {
        LiveKitLog.info(TAG + " onRequestCancelled:" + requestId + ",userId:" + userId);
        mLiveController.getSeatController().onRequestCancelled(requestId, userId);
    }

    @Override
    public void onRequestProcessed(String requestId, String userId) {
        LiveKitLog.info(TAG + " onRequestProcessed:" + requestId + ",userId:" + userId);
        mLiveController.getSeatController().onRequestProcessed(requestId, userId);
    }

    @Override
    public void onKickedOffSeat(String userId) {
        LiveKitLog.info(TAG + " onKickedOffSeat:" + userId);
        mLiveController.getSeatController().onKickedOffSeat(userId);
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info(TAG + " onUserAudioStateChanged userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason);
        mLiveController.getUserController().onUserAudioStateChanged(userId, hasAudio, reason);
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info(TAG + " onUserVideoStateChanged userId:" + userId + ",hasVideo:" + hasVideo + ",reason:"
                + reason);
        mLiveController.getUserController().onUserVideoStateChanged(userId, streamType, hasVideo, reason);
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        mLiveController.getUserController().onUserVoiceVolumeChanged(volumeMap);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info(TAG + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveController.getUserController().onRemoteUserEnterRoom(roomId, userInfo);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info(TAG + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mLiveController.getUserController().onRemoteUserLeaveRoom(roomId, userInfo);
    }

    @Override
    public void onKickedOffLine(String message) {
        LiveKitLog.info(TAG + " onKickedOffLine:[message:" + message + "]");
        ErrorHandler.handleMessage(message);
        mLiveController.getViewController().finish();
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LiveKitLog.info(TAG + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        ErrorHandler.handleMessage(message);
        mLiveController.getViewController().finish();
    }
}
