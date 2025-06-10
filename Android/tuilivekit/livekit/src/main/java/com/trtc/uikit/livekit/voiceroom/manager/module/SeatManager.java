package com.trtc.uikit.livekit.voiceroom.manager.module;

import static com.trtc.uikit.livekit.common.utils.MutableLiveDataUtils.setValue;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

import java.util.ArrayList;
import java.util.List;

public class SeatManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("SeatManager");

    public SeatManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
    }

    public void getSeatList() {
        mLiveService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                updateSeatList(list);
                mSeatState.seatList.setValue(mSeatState.seatList.getValue());
                updateSelfSeatedState();
                autoTakeSeatByOwner();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getSeatList failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void updateLinkState(SeatState.LinkStatus linkStatus) {
        mSeatState.linkStatus.setValue(linkStatus);
    }

    public void removeSentSeatInvitation(String userId) {
        mSeatState.sentSeatInvitationMap.getValue().remove(userId);
        mSeatState.sentSeatInvitationMap.setValue(mSeatState.sentSeatInvitationMap.getValue());
    }

    public void removeSeatApplication(String userId) {
        SeatState.SeatApplication application = new SeatState.SeatApplication(userId);
        mSeatState.seatApplicationList.getValue().remove(application);
        mSeatState.seatApplicationList.setValue(mSeatState.seatApplicationList.getValue());
    }

    public void addSentSeatInvitation(TUIRoomDefine.UserInfo userInfo) {
        SeatState.SeatInvitation seatInvitation = new SeatState.SeatInvitation(userInfo.userId);
        seatInvitation.updateState(userInfo);
        mSeatState.sentSeatInvitationMap.getValue().put(userInfo.userId, seatInvitation);
        mSeatState.sentSeatInvitationMap.setValue(mSeatState.sentSeatInvitationMap.getValue());
    }

    public void removeReceivedSeatInvitation() {
        SeatState.SeatInvitation seatInvitation = mSeatState.receivedSeatInvitation.getValue();
        if (!TextUtils.isEmpty(seatInvitation.userId)) {
            mSeatState.receivedSeatInvitation.getValue().reset();
            mSeatState.receivedSeatInvitation.setValue(mSeatState.receivedSeatInvitation.getValue());
        }
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mSeatState.linkStatus.setValue(SeatState.LinkStatus.LINKING);
        }
    }

    private void autoTakeSeatByOwner() {
        if (mUserState.selfInfo.userRole != TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (mSeatState.linkStatus.getValue() == SeatState.LinkStatus.LINKING) {
            return;
        }
        mLiveService.takeSeat(-1, 60, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                setValue(mSeatState.linkStatus, SeatState.LinkStatus.LINKING);
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                mSeatState.linkStatus.setValue(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                mSeatState.linkStatus.setValue(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                mSeatState.linkStatus.setValue(SeatState.LinkStatus.NONE);
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LOGGER.error("takeSeat failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
                mSeatState.linkStatus.setValue(SeatState.LinkStatus.NONE);
            }
        });
    }

    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mSeatState.seatList.getValue().contains(new SeatState.SeatInfo(selfUserId));
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
        mSeatState.seatApplicationList.getValue().add(seatApplication);
        mSeatState.seatApplicationList.setValue(mSeatState.seatApplicationList.getValue());
    }

    private void addReceivedSeatInvitation(TUIRoomDefine.UserInfo userInfo) {
        SeatState.SeatInvitation seatInvitation = new SeatState.SeatInvitation(userInfo.userId);
        seatInvitation.updateState(userInfo);
        mSeatState.receivedSeatInvitation.setValue(seatInvitation);
    }

    public void initSeatList(int maxSeatCount) {
        List<SeatState.SeatInfo> list = new ArrayList<>(maxSeatCount);
        for (int i = 0; i < maxSeatCount; i++) {
            SeatState.SeatInfo seatInfo = new SeatState.SeatInfo();
            seatInfo.index = i;
            list.add(seatInfo);
        }
        mSeatState.seatList.getValue().clear();
        mSeatState.seatList.getValue().addAll(list);
        mSeatState.seatList.setValue(mSeatState.seatList.getValue());
    }

    private void initSeatList(List<TUIRoomDefine.SeatInfo> list) {
        List<SeatState.SeatInfo> newList = new ArrayList<>(list.size());
        for (TUIRoomDefine.SeatInfo info : list) {
            SeatState.SeatInfo seatInfo = new SeatState.SeatInfo();
            seatInfo.updateState(info);
            newList.add(seatInfo);
        }
        mSeatState.seatList.getValue().clear();
        mSeatState.seatList.getValue().addAll(newList);
        mSeatState.seatList.setValue(mSeatState.seatList.getValue());
    }

    public void updateSeatList(List<TUIRoomDefine.SeatInfo> list) {
        if (list.size() != mSeatState.seatList.getValue().size()) {
            initSeatList(list);
            return;
        }
        for (int i = 0; i < list.size(); i++) {
            SeatState.SeatInfo oldSeatInfo = mSeatState.seatList.getValue().get(i);
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
        for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
            if (isSelfSeatInfo(seatInfo)) {
                mSeatState.linkStatus.setValue(SeatState.LinkStatus.NONE);
            }
        }
        for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
            if (isSelfSeatInfo(seatInfo)) {
                setValue(mSeatState.linkStatus, SeatState.LinkStatus.LINKING);
            }
        }
        if (!seatedList.isEmpty()) {
            mSeatState.seatList.setValue(mSeatState.seatList.getValue());
        }
        if (!leftList.isEmpty()) {
            mSeatState.seatList.setValue(mSeatState.seatList.getValue());
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
                R.string.common_voiceroom_kicked_out_of_seat));
    }
}
