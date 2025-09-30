package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;

import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.ConstantsKt.EVENT_SUB_KEY_DESTROY_LIVE_VIEW;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;

public class RoomEngineObserver extends TUIRoomObserver {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("RoomEngineObserver");

    protected WeakReference<AudienceManager> mLiveManager;

    public RoomEngineObserver(AudienceManager manager) {
        mLiveManager = new WeakReference<>(manager);
    }

    @Override
    public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
        LOGGER.info(hashCode() + " onRoomDismissed:[roomId" + roomId + "]");
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_room_destroy));
        TUICore.notifyEvent(
                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
        );
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.notifyOnRoomDismissed(roomId);
        }
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LOGGER.info(hashCode() + " onSeatListChanged:[seatList:" + new Gson().toJson(seatList)
                + ",seatedList:" + new Gson().toJson(seatedList) + ",leftList:" + new Gson().toJson(leftList) + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getCoGuestManager().onSeatListChanged(seatList, seatedList, leftList);
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
    public void onUserVideoSizeChanged(String roomId, String userId, TUIRoomDefine.VideoStreamType streamType,
                                       int width, int height) {
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getMediaManager().onUserVideoSizeChanged(roomId, userId, streamType, width, height);
        }
    }

    @Override
    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onUserVoiceVolumeChanged(volumeMap);
        }
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserEnterRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onRemoteUserEnterRoom(roomId, userInfo);
        }
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LOGGER.info(hashCode() + " onRemoteUserLeaveRoom:[roomId:" + roomId + ",userId:" + userInfo.userId + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onRemoteUserLeaveRoom(roomId, userInfo);
        }
    }

    @Override
    public void onKickedOffLine(String message) {
        LOGGER.info(hashCode() + " onKickedOffLine:[message:" + message + "]");
        ToastUtil.toastShortMessage(message);
        AudienceManager manager = mLiveManager.get();
        TUICore.notifyEvent(
                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
        );
        if (manager != null) {
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_LIVE_VIEW, null);
        } else {
            LOGGER.error(hashCode() + " onKickedOffLine: AudienceManager is null");
        }
    }

    @Override
    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        LOGGER.info(hashCode() + " onKickedOutOfRoom:[roomId:" + roomId + ",reason:" + reason + ",message:"
                + message + "]");
        TUICore.notifyEvent(
                TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, TUIConstants.Privacy.EVENT_SUB_KEY_ROOM_STATE_STOP, null
        );
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getRoomManager().onKickedOutOfRoom(roomId, reason);
        }
    }

    @Override
    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        LOGGER.info(hashCode() + "onUserInfoChanged:[userInfo:" + new Gson().toJson(userInfo)
                + ", modifyFlag:" + new Gson().toJson(modifyFlag));
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onUserInfoChanged(userInfo, modifyFlag);
        }
    }

    @Override
    public void onSendMessageForUserDisableChanged(String roomId, String userId, boolean isDisable) {
        LOGGER.info(hashCode() + " onSendMessageForUserDisableChanged:[roomId:" + roomId + ",userId:" + userId + "," +
                "isDisable:" + isDisable + "]");
        AudienceManager manager = mLiveManager.get();
        if (manager != null) {
            manager.getUserManager().onSendMessageForUserDisableChanged(roomId, userId, isDisable);
        }
    }
}
