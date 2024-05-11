package com.trtc.uikit.livekit.liveroom.core;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.List;

public class EngineObserver extends TUIRoomObserver {

    private final LiveRoomInfo      mLiveRoomInfo;
    private final RoomEngineService mRoomEngineService;

    public EngineObserver(LiveRoomInfo roomInfo, RoomEngineService service) {
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
    }

    @Override
    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info("EngineObserver onRemoteUserEnterRoom:[roomId:" + roomId + ",userInfo:" + userInfo + "]");
        UserInfo user = null;
        for (UserInfo info : mLiveRoomInfo.audienceList.get()) {
            if (info.userId.equals(userInfo.userId)) {
                user = info;
                break;
            }
        }
        if (user == null) {
            mLiveRoomInfo.audienceList.add(new UserInfo(userInfo));
        } else {
            user.update(userInfo);
        }
    }

    @Override
    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        LiveKitLog.info("EngineObserver onRemoteUserLeaveRoom:[roomId:" + roomId + ",userInfo:" + userInfo + "]");
        UserInfo info = new UserInfo(userInfo);
        mLiveRoomInfo.audienceList.remove(info);
        LiveKitStore.sharedInstance().applyLinkAudienceList.remove(info);
    }

    @Override
    public void onRoomDismissed(String roomId) {
        LiveKitLog.info("EngineObserver onRoomDismissed:[roomId:" + roomId + "]");
        mLiveRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.DASHBOARD);
    }

    @Override
    public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info("EngineObserver onUserAudioStateChanged:[userId:" + userId + ",hasAudio:" + hasAudio
                + ",reason:" + reason + "]");
        if (userId.equals(mLiveRoomInfo.anchorInfo.userId)) {
            mLiveRoomInfo.anchorInfo.audioInfo.muteAudio.set(!hasAudio);
        }

        for (UserInfo user : mLiveRoomInfo.linkingAudienceList.get()) {
            if (user.userId.equals(userId)) {
                user.audioInfo.muteAudio.set(!hasAudio);
                break;
            }
        }

        if (hasAudio) {
            mLiveRoomInfo.hasAudioList.add(userId);
        } else {
            mLiveRoomInfo.hasAudioList.remove(userId);
        }
    }

    @Override
    public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType, boolean hasVideo,
                                        TUIRoomDefine.ChangeReason reason) {
        LiveKitLog.info("EngineObserver onUserVideoStateChanged:[userId:" + userId + ",streamType:" + streamType
                + ",hasVideo:" + hasVideo + ",reason:" + reason + "]");
        if (userId.equals(mLiveRoomInfo.anchorInfo.userId)) {
            mLiveRoomInfo.anchorInfo.videoInfo.isCameraOpened.set(hasVideo);
        }

        for (UserInfo user : mLiveRoomInfo.linkingAudienceList.get()) {
            if (user.userId.equals(userId)) {
                user.videoInfo.isCameraOpened.set(hasVideo);
                break;
            }
        }

        if (hasVideo) {
            mLiveRoomInfo.hasVideoList.add(userId);
        } else {
            mLiveRoomInfo.hasVideoList.remove(userId);
        }
    }

    @Override
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        LiveKitLog.info("EngineObserver onSeatListChanged:[seatList=" + seatList + ",seatedList:" + seatedList
                + ",leftList:" + leftList + "]");
        mRoomEngineService.handleSeatChange(seatList, seatedList);
    }

    @Override
    public void onRequestReceived(TUIRoomDefine.Request request) {
        LiveKitLog.info("EngineObserver onRequestReceived:[request=" + request + "]");
        if (request.requestAction == TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
            UserInfo userInfo = new UserInfo(request.userId);
            userInfo.requestId = request.requestId;
            userInfo.name.set(request.userName);
            userInfo.avatarUrl.set(request.avatarUrl);
            LiveKitStore.sharedInstance().applyLinkAudienceList.add(userInfo);
        }
    }

    @Override
    public void onRequestCancelled(String requestId, String userId) {
        LiveKitLog.info("EngineObserver onRequestCancelled:[requestId=" + requestId + ",userId:" + userId + "]");
        for (UserInfo info : LiveKitStore.sharedInstance().applyLinkAudienceList.get()) {
            if (info.userId.equals(userId)) {
                LiveKitStore.sharedInstance().applyLinkAudienceList.remove(info);
                break;
            }
        }
    }

    @Override
    public void onRoomUserCountChanged(String roomId, int userCount) {
        LiveKitLog.info("EngineObserver onRoomUserCountChanged:[roomId=" + roomId + ",userCount:" + userCount + "]");
        // Anchor are not counted in the number of viewers
        userCount--;
        if (userCount > 0) {
            mLiveRoomInfo.audienceCount.set(userCount);
            if (userCount > LiveStore.sharedInstance().getLiveController().getRoomSate().maxAudienceNumber) {
                LiveStore.sharedInstance().getLiveController().getRoomController().updateMaxAudienceNumber(userCount);
            }
        }
    }
}
