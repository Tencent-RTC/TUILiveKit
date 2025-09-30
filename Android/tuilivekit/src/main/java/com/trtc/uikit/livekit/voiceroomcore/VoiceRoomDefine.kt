package com.trtc.uikit.livekit.voiceroomcore

import android.view.View
import android.view.ViewGroup.LayoutParams.WRAP_CONTENT
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine

object VoiceRoomDefine {
    enum class LayoutMode {
        FOCUS, GRID, VERTICAL, FREE
    }

    enum class SeatViewLayoutRowAlignment {
        SPACE_AROUND, SPACE_BETWEEN, SPACE_EVENLY, START, END, CENTER
    }

    enum class RequestType {
        APPLY_TO_TAKE_SEAT, INVITE_TO_TAKE_SEAT
    }

    data class Size(
        val width: Int = WRAP_CONTENT,
        val height: Int = WRAP_CONTENT
    )

    class SeatViewLayoutConfig(
        var rowConfigs: MutableList<SeatViewLayoutRowConfig> = mutableListOf(),
        var rowSpacing: Int = 10
    )

    class SeatViewLayoutRowConfig(
        var count: Int = 4,
        var seatSpacing: Int = 0,
        var seatSize: Size = Size(),
        var alignment: SeatViewLayoutRowAlignment = SeatViewLayoutRowAlignment.SPACE_AROUND
    )

    interface RequestCallback {
        fun onAccepted(userInfo: TUIRoomDefine.UserInfo)
        fun onRejected(userInfo: TUIRoomDefine.UserInfo)
        fun onCancelled(userInfo: TUIRoomDefine.UserInfo)
        fun onTimeout(userInfo: TUIRoomDefine.UserInfo)
        fun onError(userInfo: TUIRoomDefine.UserInfo, error: TUICommonDefine.Error, message: String)
    }

    interface SeatViewAdapter {
        fun createSeatView(seatGridView: SeatGridView, seatInfo: TUIRoomDefine.SeatInfo): View
        fun updateSeatView(seatGridView: SeatGridView, seatInfo: TUIRoomDefine.SeatInfo, seatView: View)
        fun updateUserVolume(seatGridView: SeatGridView, volume: Int, seatView: View)
    }
}