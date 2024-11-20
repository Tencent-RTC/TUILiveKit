package com.trtc.uikit.livekit.voiceroomcore;

import android.view.View;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public interface SeatViewAdapter {
    View createSeatView(SeatGridView seatGridView, TUIRoomDefine.SeatInfo seatInfo);

    void updateSeatView(SeatGridView seatGridView, TUIRoomDefine.SeatInfo seatInfo, View seatView);

    void updateUserVolume(SeatGridView seatGridView, int volume, View seatView);
}