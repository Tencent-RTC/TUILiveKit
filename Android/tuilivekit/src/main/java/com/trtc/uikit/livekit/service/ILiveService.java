package com.trtc.uikit.livekit.service;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveModifyFlag;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.liteav.audio.TXAudioEffectManager;

import java.util.List;

public interface ILiveService {
    void destroy();

    void addObserver(TUIRoomObserver observer);

    void removeObserver(TUIRoomObserver observer);

    /****************************************** Room Business *******************************************/
    void start(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback);

    void join(String roomId, TUIRoomDefine.GetRoomInfoCallback callback);

    void leave(TUIRoomDefine.ActionCallback callback);

    void stop(TUIRoomDefine.ActionCallback callback);

    /****************************************** Seat Business *******************************************/
    TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback);

    void leaveSeat(TUIRoomDefine.ActionCallback callback);

    void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams params, TUIRoomDefine.ActionCallback callback);

    void kickSeat(String userId, TUIRoomDefine.ActionCallback callback);

    void getSeatList(TUIRoomDefine.GetSeatListCallback callback);

    void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback);

    void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback);

    void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback);

    void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback);

    void kickUserOffSeatByAdmin(int seatIndex, String userId, TUIRoomDefine.ActionCallback callback);


    /****************************************** User Business *******************************************/
    void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback);

    void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback);

    void muteAllRemoteAudio(boolean isMute);


    /****************************************** Media Business *******************************************/
    void openLocalMicrophone(TUIRoomDefine.ActionCallback callback);

    void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                         TUIRoomDefine.ActionCallback callback);

    void closeLocalCamera();

    void setCameraMirror(boolean isMirror);

    void switchCamera(boolean isFrontCamera);

    void setLocalVideoView(TUIVideoView view);

    void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView);

    void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                              TUIRoomDefine.PlayCallback callback);

    void muteLocalAudio();

    void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback);

    void enableGravitySensor(boolean enable);

    void updateVideoQuality(TUIRoomDefine.VideoQuality quality);

    void updateAudioQuality(TUIRoomDefine.AudioQuality quality);

    void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode);

    void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type);

    void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type);

    void enableVoiceEarMonitor(boolean enable);

    void stopMusic(int id);

    void startMusic(int id, String path, Float pitch);

    void setMusicVolume(int volume);

    void setVoiceEarMonitorVolume(int volume);

    void setVoiceVolume(int volume);

    void setBeautyStyle(int style);

    void setBeautyLevel(float level);

    void setWhitenessLevel(float level);

    void setRuddyLevel(float level);

    /****************************************** IM Business *******************************************/
    void followUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback);

    void unfollowUser(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowOperationResult>> callback);

    void checkFollowType(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>> callback);

    void getUserFollowInfo(List<String> userIDList, V2TIMValueCallback<List<V2TIMFollowInfo>> callback);

    /****************************************** DATA REPORT *******************************************/
    void callExperimentalAPI(String jsonStr, Object param);
    
    /****************************************** Plugin - Room List *******************************************/
    void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback);

    void setLiveInfo(LiveInfo liveInfo, LiveModifyFlag flag, TUIRoomDefine.ActionCallback callback);

}
