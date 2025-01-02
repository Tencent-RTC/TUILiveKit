package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.ArrayList;
import java.util.List;

public class SeatActionSheetGenerator {
    private static final String FILE = "SeatActionSheetGenerator";

    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;
    private final SeatManager      mSeatManager;
    private final SeatGridView     mSeatGridView;

    private SeatInvitationDialog mSeatInvitationDialog;
    private UserManagerDialog    mUserManagerDialog;

    public SeatActionSheetGenerator(Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatManager = mVoiceRoomManager.getSeatManager();
        mSeatGridView = seatGridView;
    }

    public List<ListMenuInfo> generate(TUIRoomDefine.SeatInfo seatInfo) {
        UserState.UserInfo selfInfo = mVoiceRoomManager.getUserState().selfInfo;
        if (selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            return generaSeatManagerMenuInfo(seatInfo, selfInfo);
        } else {
            return generaSeatGeneraUserMenuInfo(seatInfo, selfInfo);
        }
    }

    private List<ListMenuInfo> generaSeatManagerMenuInfo(TUIRoomDefine.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (TextUtils.isEmpty(seatInfo.userId)) {
            if (!seatInfo.isLocked) {
                ListMenuInfo inviteUser = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_invite), () ->
                        showSeatInvitationPanel(seatInfo.index));
                menuInfoList.add(inviteUser);
            }
            ListMenuInfo lockSeat = new ListMenuInfo(seatInfo.isLocked
                    ? mContext.getString(R.string.livekit_voiceroom_unlock)
                    : mContext.getString(R.string.livekit_voiceroom_lock), () -> lockSeat(seatInfo));
            menuInfoList.add(lockSeat);
            return menuInfoList;
        }
        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            return menuInfoList;
        }
        showUserManagerPanel(seatInfo);
        return menuInfoList;
    }

    private List<ListMenuInfo> generaSeatGeneraUserMenuInfo(TUIRoomDefine.SeatInfo seatInfo,
                                                            UserState.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (seatInfo.isLocked) {
            return menuInfoList;
        }

        if (TextUtils.isEmpty(seatInfo.userId)) {
            if (mVoiceRoomManager.getSeatState().linkStatus.get() == SeatState.LinkStatus.LINKING) {
                ListMenuInfo moveSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_take_seat),
                        () -> moveToSeat(seatInfo.index));
                menuInfoList.add(moveSeat);
            } else {
                ListMenuInfo takeSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_take_seat),
                        () -> takeSeat(seatInfo.index));
                menuInfoList.add(takeSeat);
            }
            return menuInfoList;
        }
        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            return menuInfoList;
        }
        showUserManagerPanel(seatInfo);
        return menuInfoList;
    }

    private void showSeatInvitationPanel(int index) {
        if (mSeatInvitationDialog == null) {
            mSeatInvitationDialog = new SeatInvitationDialog(mContext, mVoiceRoomManager, mSeatGridView);
        }
        mSeatInvitationDialog.setInviteSeatIndex(index);
        mSeatInvitationDialog.show();
    }

    private void showUserManagerPanel(TUIRoomDefine.SeatInfo seatInfo) {
        if (mUserManagerDialog == null) {
            mUserManagerDialog = new UserManagerDialog(mContext, mVoiceRoomManager, mSeatGridView);
        }
        mUserManagerDialog.setSeatIndex(seatInfo.index);
        mUserManagerDialog.show();
    }

    private boolean isSelfSeatInfo(TUIRoomDefine.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        if (TextUtils.isEmpty(selfInfo.userId)) {
            return false;
        }
        return selfInfo.userId.equals(seatInfo.userId);
    }

    private void takeSeat(int seatIndex) {
        mSeatGridView.takeSeat(seatIndex, 60, new VoiceRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.LINKING);
            }

            @Override
            public void onRejected(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_take_seat_rejected));
            }

            @Override
            public void onCancelled(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onTimeout(TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.livekit_voiceroom_take_seat_timeout));
            }

            @Override
            public void onError(TUIRoomDefine.UserInfo userInfo, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeSeat failed,error:" + error + ",message:" + message);
                if (error != TUICommonDefine.Error.REQUEST_ID_REPEAT) {
                    mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                }
                ErrorLocalized.onError(error);
            }
        });
        mSeatManager.updateLinkState(SeatState.LinkStatus.APPLYING);
    }

    private void moveToSeat(int seatIndex) {
        mSeatGridView.moveToSeat(seatIndex, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "moveToSeat failed,error:" + error + ",message:" + message);
            }
        });
    }

    private void lockSeat(TUIRoomDefine.SeatInfo seatInfo) {
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = seatInfo.isAudioLocked;
        params.lockSeat = !seatInfo.isLocked;
        mSeatGridView.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "lockSeat failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }
}
