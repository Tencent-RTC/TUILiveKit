package com.trtc.uikit.livekit.view.voiceroom.model;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanel;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.SeatController;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.invite.SeatInvitationView;
import com.trtc.uikit.livekit.view.voiceroom.view.panel.user.UserManagerPanelView;

import java.util.ArrayList;
import java.util.List;

public class SeatActionSheetGenerator {
    private final Context        mContext;
    private final LiveController mLiveController;
    private final SeatController mSeatController;

    private BottomPanel        mSeatInvitationPanel;
    private SeatInvitationView mSeatInvitationView;
    private BottomPanel        mUserManagerPanel;

    public SeatActionSheetGenerator(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mSeatController = liveController.getSeatController();
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
            if (!seatInfo.isLocked.get()) {
                ListMenuInfo inviteUser = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_invite), () ->
                        showSeatInvitationPanel(seatInfo.index));
                menuInfoList.add(inviteUser);
            }
            ListMenuInfo lockSeat = new ListMenuInfo(seatInfo.isLocked.get()
                    ? mContext.getString(R.string.livekit_voiceroom_unlock)
                    : mContext.getString(R.string.livekit_voiceroom_lock), () -> mSeatController.lockSeat(seatInfo));
            menuInfoList.add(lockSeat);
            return menuInfoList;
        }
        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            return menuInfoList;
        }
        showUserManagerPanel(seatInfo);
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
        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            return menuInfoList;
        }
        showUserManagerPanel(seatInfo);
        return menuInfoList;
    }

    private void showSeatInvitationPanel(int index) {
        if (mSeatInvitationPanel == null) {
            mSeatInvitationView = new SeatInvitationView(mContext, mLiveController);
            mSeatInvitationPanel = BottomPanel.create(mSeatInvitationView);
        }
        mSeatInvitationView.setInviteSeatIndex(index);
        mSeatInvitationPanel.show();
    }

    private void showUserManagerPanel(SeatState.SeatInfo seatInfo) {
        if (mUserManagerPanel == null) {
            UserManagerPanelView panelView = new UserManagerPanelView(mContext, mLiveController, seatInfo);
            mUserManagerPanel = BottomPanel.create(panelView);
        }
        mUserManagerPanel.show();
    }

    private boolean isSelfSeatInfo(SeatState.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        if (TextUtils.isEmpty(selfInfo.userId)) {
            return false;
        }
        return selfInfo.userId.equals(seatInfo.userId.get());
    }
}
