package com.trtc.uikit.livekit.voiceroom.model;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.controller.MediaController;
import com.trtc.uikit.livekit.common.core.controller.SeatController;
import com.trtc.uikit.livekit.common.core.model.BottomMenuInfo;
import com.trtc.uikit.livekit.common.core.model.ListMenuInfo;
import com.trtc.uikit.livekit.common.core.store.state.operation.MediaState;
import com.trtc.uikit.livekit.common.core.store.state.operation.RoomState;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.core.store.state.operation.UserState;
import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftListView;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.GiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.voiceroom.view.panel.seatapplication.SeatApplicationPanel;

import java.util.ArrayList;
import java.util.List;

public class MenuDataGenerate {
    private final Context         mContext;
    private final LiveController  mLiveController;
    private final SeatController  mSeatController;
    private final MediaController mMediaController;

    private final MediaState mMediaState;
    private final UserState  mUserState;
    private final RoomState  mRoomState;
    private final SeatState  mSeatState;

    private       TUIGiftListView  mGiftListView;
    private final IGiftCloudServer mGiftCloudServer = new GiftCloudServer();

    public MenuDataGenerate(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        mSeatController = liveController.getSeatController();
        mMediaController = liveController.getMediaController();
        mMediaState = liveController.getMediaState();
        mUserState = liveController.getUserState();
        mRoomState = liveController.getRoomSate();
        mSeatState = liveController.getSeatState();
    }

    public List<BottomMenuInfo> generateBottomMenuData() {
        UserState.UserInfo selfInfo = mLiveController.getUserState().selfInfo;
        if (selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            return generateManagerMenuInfoList();
        } else {
            return generateAudienceMenuInfoList();
        }
    }

    public List<ListMenuInfo> generateOperateSeatMenuInfo(SeatState.SeatInfo seatInfo) {
        UserState.UserInfo selfInfo = mLiveController.getUserState().selfInfo;
        if (selfInfo.role.get() == TUIRoomDefine.Role.ROOM_OWNER) {
            return generaSeatManagerMenuInfo(seatInfo, selfInfo);
        } else {
            return generaSeatGeneraUserMenuInfo(seatInfo, selfInfo);
        }
    }

    private List<BottomMenuInfo> generateManagerMenuInfoList() {
        List<BottomMenuInfo> list = new ArrayList<>();
        BottomMenuInfo linkMic = new BottomMenuInfo(ViewState.NavigationState.LINK_MANAGEMENT,
                R.drawable.livekit_ic_link_mic, () -> {
            SeatApplicationPanel panel = new SeatApplicationPanel(mContext, mLiveController);
            panel.show();
        });
        list.add(linkMic);
        BottomMenuInfo music = new BottomMenuInfo(ViewState.NavigationState.MUSIC, R.drawable.livekit_console_music,
                () -> {
                    AudioEffectPanel panel = new AudioEffectPanel(mContext, mLiveController);
                    panel.show();
                });
        list.add(music);
        return list;
    }

    private List<BottomMenuInfo> generateAudienceMenuInfoList() {
        List<BottomMenuInfo> list = new ArrayList<>();
        BottomMenuInfo linkMic = new BottomMenuInfo(ViewState.NavigationState.LINK_MIC,
                R.drawable.livekit_ic_link_mic, this::operateTakeSeatBottomMenu);
        list.add(linkMic);

        initGiftPanelView(mRoomState.roomId);
        BottomMenuInfo gift = new BottomMenuInfo(ViewState.NavigationState.GIFT, R.drawable.livekit_ic_gift,
                () -> mGiftListView.show());
        list.add(gift);
        BottomMenuInfo like = new BottomMenuInfo(ViewState.NavigationState.LIKE, R.drawable.livekit_ic_like, () -> {

        });
        list.add(like);
        return list;
    }

    private void operateTakeSeatBottomMenu() {
        if (mUserState.selfInfo.isInSeat.get()) {
            mSeatController.leaveSeat();
            return;
        }
        String takeSeatApplicationId = mSeatState.mySeatApplicationId.get();
        if (TextUtils.isEmpty(takeSeatApplicationId)) {
            mSeatController.takeSeat(-1);
        } else {
            mSeatController.cancelTakeSeatApplication();
        }
    }

    private void initGiftPanelView(String roomId) {
        mGiftListView = new TUIGiftListView(mContext, roomId);
        mGiftListView.setListener(new TUIGiftListView.OnGiftListener() {
            @Override
            public void onRecharge(TUIGiftListView view) {
                mGiftCloudServer.rechargeBalance((error, result) -> {
                    if (error == IGiftCloudServer.Error.NO_ERROR) {
                        view.setBalance(result);
                    } else {
                        ToastUtil.toastLongMessage("recharge error, code = " + error);
                    }
                });
            }

            @Override
            public void onSendGift(TUIGiftListView view, TUIGift gift, int giftCount) {
                TUIGiftUser receiver = new TUIGiftUser();
                receiver.userId = mUserState.ownerInfo.userId;
                receiver.userName = mUserState.ownerInfo.name.get();
                receiver.avatarUrl = mUserState.ownerInfo.avatarUrl.get();
                receiver.level = "0";
                mGiftCloudServer.sendGift(TUILogin.getUserId(), receiver.userId, gift, giftCount,
                        (error, result) -> {
                            if (error == IGiftCloudServer.Error.NO_ERROR) {
                                view.sendGift(gift, giftCount, receiver);
                                view.setBalance(result);
                            } else {
                                ToastUtil.toastLongMessage("send gift error, code = " + error);
                            }
                        });
            }
        });
        mGiftCloudServer.queryGiftInfoList((error, result) -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListView.setGiftList(result);
            } else {
                ToastUtil.toastLongMessage("query gift list error, code = " + error);
            }
        });
        mGiftCloudServer.queryBalance((error, result) -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListView.setBalance(result);
            } else {
                ToastUtil.toastLongMessage("query balance error, code = " + error);
            }
        });
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

        ListMenuInfo kickSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_kick_seat),
                () -> mSeatController.kickSeat(seatInfo.userId.get()));
        menuInfoList.add(kickSeat);
        return menuInfoList;
    }

    private List<ListMenuInfo> generaSeatGeneraUserMenuInfo(SeatState.SeatInfo seatInfo, UserState.UserInfo selfInfo) {
        List<ListMenuInfo> menuInfoList = new ArrayList<>();
        if (seatInfo.isLocked.get()) {
            return menuInfoList;
        }
        if (TextUtils.isEmpty(seatInfo.userId.get())) {
            ListMenuInfo takeSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_apply_take_seat),
                    () -> mSeatController.takeSeat(seatInfo.index));
            menuInfoList.add(takeSeat);
            return menuInfoList;
        }

        if (isSelfSeatInfo(seatInfo, selfInfo)) {
            ListMenuInfo muteAudio = generaMicMenuInfo();
            menuInfoList.add(muteAudio);
            ListMenuInfo leaveSeat = new ListMenuInfo(mContext.getString(R.string.livekit_voiceroom_leave_seat),
                    mSeatController::leaveSeat);
            menuInfoList.add(leaveSeat);
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
