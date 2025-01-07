package com.trtc.uikit.livekit.voiceroomcore.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.manager.VoiceRoomManager;

import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private final String FILE = "LiveObserver[" + hashCode() + "]";

    protected VoiceRoomManager mVoiceRoomManager;

    public RoomEngineObserver(VoiceRoomManager manager) {
        mVoiceRoomManager = manager;
    }

    @Override
    public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
        Logger.info(FILE, " onRoomDismissed:[roomId" + roomId + ",reason:" + reason + "]");
        mVoiceRoomManager.getRoomManager().onRoomDismissed(roomId, reason);
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        Logger.info(FILE, " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        Logger.info(FILE, " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        mVoiceRoomManager.getSeatManager().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        Logger.info(FILE, " onRequestReceived:[request:" + new Gson().toJson(request) + "]");
        mVoiceRoomManager.getSeatManager().onRequestReceived(request);
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        Logger.info(FILE, " onRequestCancelled:[request:" + request + ",operateUser:" + operateUser + "]");
        mVoiceRoomManager.getSeatManager().onRequestCancelled(request, operateUser);
    }

    @Override
    public void onRequestProcessed(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        Logger.info(FILE, " onRequestProcessed:[requestId:" + request + ",operateUser:" + operateUser + "]");
        mVoiceRoomManager.getSeatManager().onRequestProcessed(request, operateUser);
    }

    @Override
    public void onKickedOffSeat(int seatIndex, TUIRoomDefine.UserInfo operateUser) {
        Logger.info(FILE, " onKickedOffSeat:[seatIndex:" + seatIndex + "]");
        mVoiceRoomManager.getSeatManager().onKickedOffSeat(seatIndex, operateUser);
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        Logger.info(FILE, " onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
        mVoiceRoomManager.getUserManager().onUserAudioStateChanged(userId, hasAudio, reason);
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        mVoiceRoomManager.getUserManager().onUserVoiceVolumeChanged(volumeMap);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
    }

    @Override
    public void onKickedOffLine(String message) {
        Logger.info(FILE, " onKickedOffLine:[message:" + message + "]");
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        Logger.info(FILE, " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        mVoiceRoomManager.getRoomManager().onKickedOutOfRoom(roomId, reason, message);
    }
}
