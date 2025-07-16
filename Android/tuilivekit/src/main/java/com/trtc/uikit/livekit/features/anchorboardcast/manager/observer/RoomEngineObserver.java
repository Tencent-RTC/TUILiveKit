package com.trtc.uikit.livekit.features.anchorboardcast.manager.observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("RoomEngineObserver");

    protected WeakReference<AnchorManager> mLiveManager;

    public RoomEngineObserver(AnchorManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
        LOGGER.info(hashCode() + " onRoomDismissed:[roomId" + roomId + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.onRoomDismissed(roomId);
        }
    }


    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LOGGER.info(hashCode() + " onRoomUserCountChanged:[roomId:" + roomId + ",userCount:" + userCount + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getRoomManager().onRoomUserCountChanged(roomId, userCount);
        }
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LOGGER.info(hashCode() + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onSeatLockStateChanged(seatList);
        }
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        LOGGER.info(hashCode() + " onRequestReceived:[request:" + new Gson().toJson(request) + "]");
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        LOGGER.info(hashCode() + " onRequestCancelled:[request:" + request + ",operateUser:" + operateUser + "]");
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        LOGGER.info(hashCode() + " onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo
            , TUIRoomDefine.ChangeReason reason) {
        LOGGER.info(hashCode() + " onUserVideoStateChanged:[userId:" + userId + ",hasVideo:" + hasVideo + ",reason:"
                + reason + "]");
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onUserVoiceVolumeChanged(volumeMap);
        }
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onRemoteUserEnterRoom(roomId, userInfo);
        }
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onRemoteUserLeaveRoom(roomId, userInfo);
        }
    }

    @Override
    public void onKickedOffLine(String message) {
        LOGGER.info(hashCode() + " onKickedOffLine:[message:" + message + "]");
        ToastUtil.toastShortMessage(message);
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.onRoomDismissed(manager.getRoomState().roomId);
        }
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LOGGER.info(hashCode() + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.onKickedOutOfRoom(roomId, reason, message);
        }
    }

    @Override
    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        LOGGER.info(hashCode() + "onUserInfoChanged:[userInfo:" + new Gson().toJson(userInfo)
                + ", modifyFlag:" + new Gson().toJson(modifyFlag));
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onUserInfoChanged(userInfo, modifyFlag);
        }
    }

    @Override
    public void onSendMessageForUserDisableChanged(String roomId, String userId, boolean isDisable) {
        LOGGER.info(hashCode() + " onSendMessageForUserDisableChanged:[roomId:" + roomId + ",userId:" + userId + "," +
                "isDisable:" + isDisable + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onSendMessageForUserDisableChanged(roomId, userId, isDisable);
        }
    }

    @Override
    public void onError(TUICommonDefine.Error errorCode, String message) {
        LOGGER.info(hashCode() + " onError:[errorCode:" + errorCode + ",message:" + message + "]");
        AnchorManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getMediaManager().onError(errorCode, errorCode);
        }
    }
}
