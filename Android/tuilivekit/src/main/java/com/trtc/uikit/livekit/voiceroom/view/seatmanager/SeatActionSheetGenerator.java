package com.trtc.uikit.livekit.voiceroom.view.seatmanager;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.manager.module.SeatManager;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import io.trtc.tuikit.atomicxcore.api.CompletionHandler;
import io.trtc.tuikit.atomicxcore.api.LiveSeatStore;

public class SeatActionSheetGenerator {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("SeatActionSheetGenerator");

    private final Context          mContext;
    private final VoiceRoomManager mVoiceRoomManager;
    private final SeatManager      mSeatManager;
    private final SeatGridView     mSeatGridView;
    private final LiveSeatStore    mLiveSeatStore;

    private SeatInvitationDialog mSeatInvitationDialog;
    private UserManagerDialog    mUserManagerDialog;


    public SeatActionSheetGenerator(Context context, VoiceRoomManager voiceRoomManager, SeatGridView seatGridView) {
        mContext = context;
        mVoiceRoomManager = voiceRoomManager;
        mSeatManager = voiceRoomManager.getSeatManager();
        mSeatGridView = seatGridView;
        mLiveSeatStore = LiveSeatStore.create(voiceRoomManager.getRoomState().roomId);
    }

    public List<ListMenuInfo> generate(TUIRoomDefine.SeatInfo seatInfo) {
        TUIRoomDefine.UserInfo selfInfo = mVoiceRoomManager.getUserState().selfInfo;
        if (selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
            return generaSeatManagerMenuInfo(seatInfo, selfInfo);
        } else {
            return generaSeatGeneraUserMenuInfo(seatInfo, selfInfo);
        }
    }

    public void destroy() {
        if (mUserManagerDialog != null) {
            mUserManagerDialog.dismiss();
        }
    }

    private List<ListMenuInfo> generaSeatManagerMenuInfo(TUIRoomDefine.SeatInfo seatInfo,
                                                         TUIRoomDefine.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (TextUtils.isEmpty(seatInfo.userId)) {
            if (!seatInfo.isLocked) {
                ListMenuInfo inviteUser = new ListMenuInfo(mContext.getString(R.string.common_voiceroom_invite), () ->
                        showSeatInvitationPanel(seatInfo.index));
                menuInfoList.add(inviteUser);
            }
            ListMenuInfo lockSeat = new ListMenuInfo(seatInfo.isLocked
                    ? mContext.getString(R.string.common_voiceroom_unlock)
                    : mContext.getString(R.string.common_voiceroom_lock), () -> lockSeat(seatInfo));
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
                                                            TUIRoomDefine.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (seatInfo.isLocked) {
            return menuInfoList;
        }

        if (TextUtils.isEmpty(seatInfo.userId)) {
            if (mVoiceRoomManager.getSeatState().linkStatus.getValue() == SeatState.LinkStatus.LINKING) {
                ListMenuInfo moveSeat = new ListMenuInfo(mContext.getString(R.string.common_voiceroom_take_seat),
                        () -> moveToSeat(seatInfo.index));
                menuInfoList.add(moveSeat);
            } else {
                ListMenuInfo takeSeat = new ListMenuInfo(mContext.getString(R.string.common_voiceroom_take_seat),
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

    private boolean isSelfSeatInfo(TUIRoomDefine.SeatInfo seatInfo, TUIRoomDefine.UserInfo selfInfo) {
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
                        R.string.common_voiceroom_take_seat_rejected));
            }

            @Override
            public void onCancelled(@NotNull TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onTimeout(@NotNull TUIRoomDefine.UserInfo userInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE);
                ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                        R.string.common_voiceroom_take_seat_timeout));
            }

            @Override
            public void onError(@NotNull TUIRoomDefine.UserInfo userInfo, @NotNull TUICommonDefine.Error error,
                                @NotNull String message) {
                LOGGER.error("takeSeat failed,error:" + error + ",message:" + message);
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
                LOGGER.error("moveToSeat failed,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void lockSeat(TUIRoomDefine.SeatInfo seatInfo) {
        if (seatInfo.isLocked) {
            mLiveSeatStore.unlockSeat(seatInfo.index, new CompletionHandler() {
                @Override
                public void onSuccess() {
                    LOGGER.info("unlockSeat success");
                }

                @Override
                public void onFailure(int code, @NotNull String desc) {
                    LOGGER.error("unlockSeat failed,error:" + code + ",message:" + desc);
                }
            });
        } else {
            mLiveSeatStore.lockSeat(seatInfo.index, new CompletionHandler() {
                @Override
                public void onSuccess() {
                    LOGGER.info("lockSeat success");
                }

                @Override
                public void onFailure(int code, @NotNull String desc) {
                    LOGGER.error("lockSeat failed,error:" + code + ",message:" + desc);
                }
            });
        }
    }
}
