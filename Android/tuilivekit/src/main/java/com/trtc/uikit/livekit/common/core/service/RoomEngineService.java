package com.trtc.uikit.livekit.common.core.service;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.tencent.liteav.audio.TXAudioEffectManager;

public class RoomEngineService {
    protected TUIRoomEngine mTUIRoomEngine = TUIRoomEngine.sharedInstance();

    public void addObserver(TUIRoomObserver observer) {
        mTUIRoomEngine.addObserver(observer);
    }

    public void removeObserver(TUIRoomObserver observer) {
        mTUIRoomEngine.removeObserver(observer);
    }

    /****************************************** Room Business *******************************************/
    public void start(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                enterRoom(roomInfo.roomId, callback);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void join(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        enterRoom(roomId, callback);
    }

    public void leave(TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.exitRoom(true, callback);
    }

    public void stop(TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.destroyRoom(callback);
    }

    private void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.createRoom(roomInfo, callback);
    }

    private void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        mTUIRoomEngine.enterRoom(roomId, TUIRoomDefine.RoomType.LIVE, callback);
    }

    /****************************************** Seat Business *******************************************/
    public TUIRoomDefine.Request takeSeat(int seatIndex, int timeout, TUIRoomDefine.RequestCallback callback) {
        return mTUIRoomEngine.takeSeat(seatIndex, timeout, callback);
    }

    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.leaveSeat(callback);
    }

    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams params, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.lockSeatByAdmin(seatIndex, params, callback);
    }

    public void kickSeat(String userId, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.kickUserOffSeatByAdmin(-1, userId, callback);
    }

    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {
        mTUIRoomEngine.getSeatList(callback);
    }

    public void getSeatApplicationList(TUIRoomDefine.RequestListCallback callback) {
        mTUIRoomEngine.getSeatApplicationList(callback);
    }

    public void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.responseRemoteRequest(requestId, true, callback);
    }

    public void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.responseRemoteRequest(requestId, false, callback);
    }

    public void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.cancelRequest(requestId, callback);
    }


    /****************************************** User Business *******************************************/
    public void getUserList(long nextSequence, TUIRoomDefine.GetUserListCallback callback) {
        mTUIRoomEngine.getUserList(nextSequence, callback);
    }

    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        mTUIRoomEngine.getUserInfo(userId, callback);
    }


    /****************************************** Media Business *******************************************/
    public void openLocalMicrophone(TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.openLocalMicrophone(TUIRoomDefine.AudioQuality.DEFAULT, callback);
    }

    public void closeLocalCamera() {
        mTUIRoomEngine.closeLocalCamera();
    }

    public void muteLocalAudio() {
        mTUIRoomEngine.muteLocalAudio();
    }

    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        mTUIRoomEngine.unmuteLocalAudio(callback);
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setVoiceChangerType(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setVoiceReverbType(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().enableVoiceEarMonitor(enable);
    }

    public void stopMusic(int id) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().stopPlayMusic(id);
    }

    public void startMusic(int id, String path, Float pitch, int musicVolume) {
        TXAudioEffectManager.AudioMusicParam audioMusicParam = new TXAudioEffectManager.AudioMusicParam(id, path);
        audioMusicParam.loopCount = Integer.MAX_VALUE;
        audioMusicParam.publish = true;
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().startPlayMusic(audioMusicParam);
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setAllMusicVolume(musicVolume);
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setMusicPitch(id, pitch);
    }

    public void setMusicVolume(int volume) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setAllMusicVolume(volume);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setVoiceEarMonitorVolume(volume);
    }

    public void setVoiceVolume(int volume) {
        mTUIRoomEngine.getTRTCCloud().getAudioEffectManager().setVoiceCaptureVolume(volume);
    }
}
