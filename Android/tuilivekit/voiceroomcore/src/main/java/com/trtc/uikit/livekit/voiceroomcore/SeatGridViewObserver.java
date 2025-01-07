package com.trtc.uikit.livekit.voiceroomcore;

import android.view.View;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public abstract class SeatGridViewObserver {
    public void onRoomDismissed(String roomId) {
    }

    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
    }

    public void onSeatRequestReceived(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
    }

    public void onSeatRequestCancelled(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
    }

    public void onKickedOffSeat(TUIRoomDefine.UserInfo userInfo) {
    }

    public void onUserAudioStateChanged(TUIRoomDefine.UserInfo userInfo, boolean hasAudio,
                                        TUIRoomDefine.ChangeReason reason) {
    }

    public void onSeatViewClicked(View seatView, TUIRoomDefine.SeatInfo seatInfo) {
    }
}
