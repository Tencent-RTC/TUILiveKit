package com.trtc.uikit.livekit.voiceroomcore.manager.api;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;

public interface IVoiceRoomService {

    void addRoomEngineObserver(TUIRoomObserver observer);

    void removeRoomEngineObserver(TUIRoomObserver observer);

    /****************************************** Room Business *******************************************/
    void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback);

    void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback);

    void exitRoom(TUIRoomDefine.ActionCallback callback);

    void destroyRoom(TUIRoomDefine.ActionCallback callback);

    void updateRoomSeatMode(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback);

    /****************************************** Seat Business *******************************************/
    TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback);

    void moveToSeat(int seatIndex, TUIRoomDefine.ActionCallback callback);

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

    void kickUserOffSeatByAdmin(String userId, TUIRoomDefine.ActionCallback callback);

    /****************************************** User Business *******************************************/

    void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback);

    /****************************************** Media Business *******************************************/
    void openLocalMicrophone(TUIRoomDefine.ActionCallback callback);

    void closeLocalMicrophone();

    void muteLocalAudio();

    void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback);

    /****************************************** DATA REPORT *******************************************/
    void callExperimentalAPI(String jsonStr);
}
