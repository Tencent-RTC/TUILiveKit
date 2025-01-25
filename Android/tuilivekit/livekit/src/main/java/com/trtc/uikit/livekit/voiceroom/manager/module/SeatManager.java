package com.trtc.uikit.livekit.voiceroom.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.ArrayList;
import java.util.List;

public class SeatManager extends BaseManager {
    private static final String FILE = "SeatManager";

    public SeatManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        Logger.info(FILE, " destroy");
    }

    public void getSeatList() {
        mLiveService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                updateSeatList(list);
                mSeatState.seatList.notifyDataChanged();
                updateSelfSeatedState();
                autoTakeSeatByOwner();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getSeatList, error: " + error + ", message: " + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void updateLinkState(SeatState.LinkStatus linkStatus) {
        mSeatState.linkStatus.set(linkStatus, false);
    }

    public void removeSentSeatInvitation(String userId) {
        mSeatState.sentSeatInvitationMap.remove(userId);
    }

    public void removeSeatApplication(String userId) {
        SeatState.SeatApplication application = new SeatState.SeatApplication(userId);
        mSeatState.seatApplicationList.remove(application);
    }

    public void addSentSeatInvitation(UserState.UserInfo userInfo) {
        SeatState.SeatInvitation seatInvitation = new SeatState.SeatInvitation(userInfo.userId);
        seatInvitation.updateState(userInfo);
        mSeatState.sentSeatInvitationMap.put(userInfo.userId, seatInvitation);
    }

    public void removeReceivedSeatInvitation() {
        SeatState.SeatInvitation seatInvitation = mSeatState.receivedSeatInvitation.get();
        if (!TextUtils.isEmpty(seatInvitation.userId)) {
            mSeatState.receivedSeatInvitation.get().reset();
            mSeatState.receivedSeatInvitation.notifyDataChanged();
        }
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mSeatState.linkStatus.set(SeatState.LinkStatus.LINKING, false);
        }
    }

    private void autoTakeSeatByOwner() {
        if (mUserState.selfInfo.role.get() != TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (mSeatState.linkStatus.get() == SeatState.LinkStatus.LINKING) {
            return;
        }
        mLiveService.takeSeat(-1, 60, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.LINKING, false);
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.NONE, false);
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.NONE, false);
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.NONE, false);
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeSeat,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
                mSeatState.linkStatus.set(SeatState.LinkStatus.NONE, false);
            }
        });
    }

    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mSeatState.seatList.get().contains(new SeatState.SeatInfo(selfUserId));
    }

    private boolean isSelfSeatInfo(TUIRoomDefine.SeatInfo seatInfo) {
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mUserState.selfInfo.userId.equals(seatInfo.userId);
    }

    private void addSeatApplication(TUIRoomDefine.UserInfo userInfo) {
        SeatState.SeatApplication seatApplication = new SeatState.SeatApplication(userInfo.userId);
        seatApplication.updateState(userInfo);
        mSeatState.seatApplicationList.add(seatApplication);
    }

    private void addReceivedSeatInvitation(TUIRoomDefine.UserInfo userInfo) {
        SeatState.SeatInvitation seatInvitation = new SeatState.SeatInvitation(userInfo.userId);
        seatInvitation.updateState(userInfo);
        mSeatState.receivedSeatInvitation.set(seatInvitation);
    }

    public void initSeatList(int maxSeatCount) {
        List<SeatState.SeatInfo> list = new ArrayList<>(maxSeatCount);
        for (int i = 0; i < maxSeatCount; i++) {
            SeatState.SeatInfo seatInfo = new SeatState.SeatInfo();
            seatInfo.index = i;
            list.add(seatInfo);
        }
        mSeatState.seatList.get().clear();
        mSeatState.seatList.addAll(list);
    }

    private void initSeatList(List<TUIRoomDefine.SeatInfo> list) {
        List<SeatState.SeatInfo> newList = new ArrayList<>(list.size());
        for (TUIRoomDefine.SeatInfo info : list) {
            SeatState.SeatInfo seatInfo = new SeatState.SeatInfo();
            seatInfo.updateState(info);
            newList.add(seatInfo);
        }
        mSeatState.seatList.get().clear();
        mSeatState.seatList.addAll(newList);
    }

    public void updateSeatList(List<TUIRoomDefine.SeatInfo> list) {
        if (list.size() != mSeatState.seatList.get().size()) {
            initSeatList(list);
            return;
        }
        for (int i = 0; i < list.size(); i++) {
            SeatState.SeatInfo oldSeatInfo = mSeatState.seatList.get().get(i);
            TUIRoomDefine.SeatInfo newSeatInfo = list.get(i);
            if (oldSeatInfo == null || newSeatInfo == null) {
                continue;
            }
            oldSeatInfo.updateState(newSeatInfo);
        }
    }

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        updateSeatList(seatList);
        for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
            if (isSelfSeatInfo(seatInfo)) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.LINKING, false);
            }
        }
        if (!seatedList.isEmpty()) {
            mSeatState.seatList.notifyDataChanged();
        }
        for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
            if (isSelfSeatInfo(seatInfo)) {
                mSeatState.linkStatus.set(SeatState.LinkStatus.NONE, false);
            }
        }
        if (!leftList.isEmpty()) {
            mSeatState.seatList.notifyDataChanged();
        }
    }

    public void onSeatRequestReceived(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        switch (type) {
            case APPLY_TO_TAKE_SEAT:
                addSeatApplication(userInfo);
                break;
            case INVITE_TO_TAKE_SEAT:
                addReceivedSeatInvitation(userInfo);
                break;
            default:
                break;
        }
    }

    public void onSeatRequestCancelled(VoiceRoomDefine.RequestType type, TUIRoomDefine.UserInfo userInfo) {
        switch (type) {
            case APPLY_TO_TAKE_SEAT:
                removeSeatApplication(userInfo.userId);
                break;
            case INVITE_TO_TAKE_SEAT:
                removeReceivedSeatInvitation();
                break;
            default:
                break;
        }
    }

    public void onKickedOffSeat(TUIRoomDefine.UserInfo userInfo) {
        ToastUtil.toastShortMessage(TUIConfig.getAppContext().getString(
                R.string.livekit_voiceroom_kicked_out_of_seat));
    }
}
