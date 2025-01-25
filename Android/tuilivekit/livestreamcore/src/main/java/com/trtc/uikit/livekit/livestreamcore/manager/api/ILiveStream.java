package com.trtc.uikit.livekit.livestreamcore.manager.api;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.trtc.TRTCCloud;

import java.util.List;

public interface ILiveStream {
    void destroy();

    void addRoomEngineObserver(TUIRoomObserver observer);

    void removeRoomEngineObserver(TUIRoomObserver observer);

    void addLiveConnectionManagerObserver(TUILiveConnectionManager.Observer observer);

    void removeLiveConnectionManagerObserver(TUILiveConnectionManager.Observer observer);

    void addLiveBattleManagerObserver(TUILiveBattleManager.Observer observer);

    void removeLiveBattleManagerObserver(TUILiveBattleManager.Observer observer);

    void addLiveLayoutManagerObserver(TUILiveLayoutManager.Observer observer);
    
    void removeLiveLayoutManagerObserver(TUILiveLayoutManager.Observer observer);

    /****************************************** Room Business *******************************************/
    void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback);

    void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback);

    void exitRoom(TUIRoomDefine.ActionCallback callback);

    void destroyRoom(TUIRoomDefine.ActionCallback callback);

    /****************************************** Seat Business *******************************************/
    TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback);

    TUIRoomDefine.Request takeUserOnSeatByAdmin(int seatIndex, String userId, int timeout,
                                                TUIRoomDefine.RequestCallback callback);

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

    void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback);

    /****************************************** Media Business *******************************************/
    void openLocalMicrophone(TUIRoomDefine.ActionCallback callback);

    void closeLocalMicrophone();

    void openLocalCamera(boolean isFront, TUIRoomDefine.VideoQuality quality,
                         TUIRoomDefine.ActionCallback callback);

    void closeLocalCamera();

    void switchCamera(boolean isFrontCamera);

    void setLocalVideoView(TUIVideoView view);

    void setRemoteVideoView(String userId, TUIRoomDefine.VideoStreamType streamType, TUIVideoView videoView);

    void startPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType,
                              TUIRoomDefine.PlayCallback callback);

    void stopPlayRemoteVideo(String userId, TUIRoomDefine.VideoStreamType streamType);

    void muteLocalAudio();

    void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback);

    void enableGravitySensor(boolean enable);

    void setVideoResolutionMode(TUIRoomDefine.ResolutionMode resolutionMode);

    void setBeautyStyle(int style);

    void updateVideoQualityEx(TUIRoomDefine.RoomVideoEncoderParams videoEncParam);

    void updateVideoQuality(TUIRoomDefine.VideoQuality quality);

    void updateAudioQuality(TUIRoomDefine.AudioQuality quality);

    void setCameraMirror(boolean isMirror);

    /****************************************** DATA REPORT *******************************************/
    void callExperimentalAPI(String jsonStr);

    /****************************************** TRTC Cloud *******************************************/
    TRTCCloud getTRTCCloud();

    /***************************************** Plugin - Connection ******************************************/
    void requestConnection(List<String> roomIdList, int timeoutSeconds, String extensionInfo,
                           TUILiveConnectionManager.ConnectionRequestCallback callback);

    void acceptConnection(String roomId, TUIRoomDefine.ActionCallback callback);

    void rejectConnection(String roomId, TUIRoomDefine.ActionCallback callback);

    void disconnect(TUIRoomDefine.ActionCallback callback);

    void cancelConnectionRequest(List<String> list, TUIRoomDefine.ActionCallback callback);

    /***************************************** Plugin - Battle ******************************************/
    void requestBattle(TUILiveBattleManager.BattleConfig config, List<String> userIdList, int timeout,
                       TUILiveBattleManager.BattleRequestCallback callback);

    void cancelBattleRequest(String battleId, List<String> userIdList, TUIRoomDefine.ActionCallback callback);

    void acceptBattle(String battleId, TUIRoomDefine.ActionCallback callback);

    void rejectBattle(String battleId, TUIRoomDefine.ActionCallback callback);

    void exitBattle(String battleId, TUIRoomDefine.ActionCallback callback);
}
