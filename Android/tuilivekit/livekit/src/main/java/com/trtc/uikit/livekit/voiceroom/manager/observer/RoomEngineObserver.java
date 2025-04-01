package com.trtc.uikit.livekit.voiceroom.manager.observer;

import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.voiceroom.manager.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.List;

public class RoomEngineObserver extends TUIRoomObserver {
    private final String FILE = "LiveObserver[" + hashCode() + "]";

    protected VoiceRoomManager mVoiceRoomManager;

    public RoomEngineObserver(VoiceRoomManager liveController) {
        mVoiceRoomManager = liveController;
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        Logger.info(FILE, " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
        mVoiceRoomManager.getRoomManager().onRoomUserCountChanged(roomId, userCount);
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        Logger.info(FILE, " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        mVoiceRoomManager.getSeatManager().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mVoiceRoomManager.getUserManager().onRemoteUserEnterRoom(roomId, userInfo);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        Logger.info(FILE, " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mVoiceRoomManager.getUserManager().onRemoteUserLeaveRoom(roomId, userInfo);
    }

    @Override
    public void onKickedOffLine(String message) {
        Logger.info(FILE, " onKickedOffLine:[message:" + message + "]");
        ToastUtil.toastShortMessage(message);
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, null);
    }
}
