package com.trtc.uikit.livekit.voiceroom.manager.observer;

import static com.trtc.uikit.livekit.voiceroom.manager.api.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("RoomEngineObserver");

    protected VoiceRoomManager mVoiceRoomManager;

    public RoomEngineObserver(VoiceRoomManager liveController) {
        mVoiceRoomManager = liveController;
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LOGGER.info(hashCode() + " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
        mVoiceRoomManager.getRoomManager().onRoomUserCountChanged(roomId, userCount);
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LOGGER.info(hashCode() + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        mVoiceRoomManager.getSeatManager().onSeatListChanged(seatList, seatedList, leftList);
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mVoiceRoomManager.getUserManager().onRemoteUserEnterRoom(roomId, userInfo);
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        mVoiceRoomManager.getUserManager().onRemoteUserLeaveRoom(roomId, userInfo);
    }

    @Override
    public void onKickedOffLine(String message) {
        LOGGER.info(hashCode() + " onKickedOffLine:[message:" + message + "]");
        ToastUtil.toastShortMessage(message);
        if (mVoiceRoomManager != null) {
            Map<String, Object> params = new HashMap<>();
            params.put("roomId", mVoiceRoomManager.getRoomState().roomId);
            TUICore.notifyEvent(Constants.EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, params);
        } else {
            LOGGER.error(hashCode() + " onKickedOffLine: mVoiceRoomManager is null");
        }
    }
}
