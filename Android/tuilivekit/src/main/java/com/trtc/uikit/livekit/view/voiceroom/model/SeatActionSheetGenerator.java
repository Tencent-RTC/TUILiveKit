package com.trtc.uikit.livekit.view.voiceroom.model;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.MediaController;
import com.trtc.uikit.livekit.manager.controller.SeatController;
import com.trtc.uikit.livekit.state.operation.MediaState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.ArrayList;
import java.util.List;

public class SeatActionSheetGenerator {
    private final Context         mContext;
    private final LiveController  mLiveController;
    private final SeatController  mSeatController;
    private final MediaController mMediaController;

    private final MediaState mMediaState;

    public SeatActionSheetGenerator(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mSeatController = liveController.getSeatController();
        mMediaController = liveController.getMediaController();
        mMediaState = liveController.getMediaState();
    }

    public List<ListMenuInfo> generate(SeatState.SeatInfo seatInfo) {
        UserState.UserInfo selfInfo = mLiveController.getUserState().selfInfo;
        if (selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            return generaSeatManagerMenuInfo(seatInfo, selfInfo);
        } else {
            return generaSeatGeneraUserMenuInfo(seatInfo, selfInfo);
        }
    }

    private List<ListMenuInfo> generaSeatManagerMenuInfo(SeatState.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (TextUtils.isEmpty(seatInfo.userId.get())) {
            ListMenuInfo lockSeat = new ListMenuInfo(seatInfo.isLocked.get()
                    ? mContext.getString(R.string.livekit_voiceroom_unlock)
                    : mContext.getString(R.string.livekit_voiceroom_lock), () -> mSeatController.lockSeat(seatInfo));
            menuInfoList.add(lockSeat);
            return menuInfoList;
        }
        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            ListMenuInfo muteAudio = generaMicMenuInfo();
            menuInfoList.add(muteAudio);
            return menuInfoList;
        }
        ListMenuInfo muteSeat = new ListMenuInfo(seatInfo.isAudioLocked.get()
                ? mContext.getString(R.string.livekit_cvoiceroom_seat_unmuted) :
                mContext.getString(R.string.livekit_voiceroom_seat_mute),
                () -> mSeatController.muteSeatAudio(seatInfo));
        menuInfoList.add(muteSeat);
        return menuInfoList;
    }

    private List<ListMenuInfo> generaSeatGeneraUserMenuInfo(SeatState.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (seatInfo.isLocked.get()) {
            return menuInfoList;
        }
        if (TextUtils.isEmpty(seatInfo.userId.get())) {
            ListMenuInfo takeSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_take_seat),
                    () -> mSeatController.takeSeat(seatInfo.index));
            menuInfoList.add(takeSeat);
            return menuInfoList;
        }
        return menuInfoList;
    }

    private ListMenuInfo generaMicMenuInfo() {
        return new ListMenuInfo(mMediaState.isMicrophoneMuted.get()
                ? mContext.getString(R.string.livekit_voiceroom_unmute_mic) :
                mContext.getString(R.string.livekit_voiceroom_mute_mic), mMediaController::operateMicrophone);
    }

    private boolean isSelfSeatInfo(SeatState.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        if (TextUtils.isEmpty(selfInfo.userId)) {
            return false;
        }
        return selfInfo.userId.equals(seatInfo.userId.get());
    }
}
