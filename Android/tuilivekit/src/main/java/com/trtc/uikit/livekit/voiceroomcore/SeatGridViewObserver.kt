package com.trtc.uikit.livekit.voiceroomcore

import android.view.View
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.RequestType

abstract class SeatGridViewObserver {
    open fun onRoomDismissed(roomId: String) = Unit
    open fun onKickedOutOfRoom(roomId: String, reason: TUIRoomDefine.KickedOutOfRoomReason, message: String) = Unit
    open fun onSeatRequestReceived(type: RequestType, userInfo: TUIRoomDefine.UserInfo) = Unit
    open fun onSeatRequestCancelled(type: RequestType, userInfo: TUIRoomDefine.UserInfo) = Unit
    open fun onKickedOffSeat(userInfo: TUIRoomDefine.UserInfo) = Unit
    open fun onSeatViewClicked(seatView: View, seatInfo: TUIRoomDefine.SeatInfo) = Unit
}
