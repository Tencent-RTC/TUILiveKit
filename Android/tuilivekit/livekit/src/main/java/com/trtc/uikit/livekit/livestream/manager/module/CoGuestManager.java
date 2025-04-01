package com.trtc.uikit.livekit.livestream.manager.module;

import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.LINKING;
import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.NONE;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CoGuestManager extends BaseManager {
    private static final String TAG = "CoGuestManager";

    private Map<String, TUIRoomDefine.SeatInfo> seatInfoMap = new HashMap<>();

    public CoGuestManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    public enum MediaDevice {
        MICROPHONE,
        CAMERA
    }

    @Override
    public void destroy() {
    }

    public void getSeatList() {
        mLiveService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                List<UserInfo> userList = new ArrayList<>();
                for (TUIRoomDefine.SeatInfo info : list) {
                    UserInfo user = new UserInfo();
                    user.userId = info.userId;
                    user.userName = info.userName;
                    user.avatarUrl = info.avatarUrl;
                    userList.add(user);
                }
                initSeatList(userList);
                mCoGuestState.connectedUserList.setValue(mCoGuestState.connectedUserList.getValue());
                updateSelfSeatedState();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(TAG + " getUserList failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void disableUserMediaDevice(String userId, MediaDevice device) {
        TUIRoomDefine.SeatInfo seatInfo = seatInfoMap.get(userId);
        if (seatInfo == null) {
            return;
        }
        boolean isAudioLocked = seatInfo.isAudioLocked;
        boolean isVideoLocked = seatInfo.isVideoLocked;
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        if (device == MediaDevice.CAMERA) {
            params.lockVideo = !isVideoLocked;
            params.lockAudio = isAudioLocked;
        } else {
            params.lockAudio = !isAudioLocked;
            params.lockVideo = isVideoLocked;
        }
        mLiveService.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveStreamLog.info(TAG + " lockSeat:[success]");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveStreamLog.error(TAG + " lockSeat failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    private void initSeatList(List<TUIRoomDefine.UserInfo> list) {
        List<CoGuestState.SeatInfo> newList = new ArrayList<>(list.size());
        for (UserInfo info : list) {
            if (TextUtils.isEmpty(info.userId)) {
                continue;
            }
            CoGuestState.SeatInfo seatInfo = new CoGuestState.SeatInfo();
            seatInfo.updateState(info);
            newList.add(seatInfo);
        }
        mCoGuestState.connectedUserList.getValue().clear();
        mCoGuestState.connectedUserList.getValue().addAll(newList);
        mCoGuestState.connectedUserList.setValue(mCoGuestState.connectedUserList.getValue());
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mCoGuestState.coGuestStatus.setValue(LINKING);
        }
    }

    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mCoGuestState.connectedUserList.getValue().contains(new CoGuestState.SeatInfo(selfUserId));
    }

    private boolean isSelfSeatInfo(UserInfo seatInfo) {
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mUserState.selfInfo.userId.equals(seatInfo.userId);
    }

    public void updateCoGuestStates(CoGuestState.CoGuestStatus linkStatus) {
        mCoGuestState.coGuestStatus.setValue(linkStatus);
    }

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<UserInfo> userList, List<UserInfo> joinList,
                                  List<UserInfo> leaveList) {
        initSeatList(userList);
        for (UserInfo seatInfo : joinList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.setValue(LINKING);
            }
        }
        if (!joinList.isEmpty()) {
            mCoGuestState.connectedUserList.setValue(mCoGuestState.connectedUserList.getValue());
        }
        for (UserInfo seatInfo : leaveList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.setValue(NONE);
            }
        }
        if (!leaveList.isEmpty()) {
            mCoGuestState.connectedUserList.setValue(mCoGuestState.connectedUserList.getValue());
        }
    }

    public void onSeatLockStateChanged(List<TUIRoomDefine.SeatInfo> seatList) {
        seatInfoMap.clear();
        for (TUIRoomDefine.SeatInfo seatInfo : seatList) {
            if (TextUtils.isEmpty(seatInfo.userId)) {
                continue;
            }

            seatInfoMap.put(seatInfo.userId, seatInfo);
            if (seatInfo.isAudioLocked) {
                mCoGuestState.lockAudioUserList.getValue().add(seatInfo.userId);
            } else {
                mCoGuestState.lockAudioUserList.getValue().remove(seatInfo.userId);
            }
            mCoGuestState.lockAudioUserList.setValue(mCoGuestState.lockAudioUserList.getValue());
            if (seatInfo.isVideoLocked) {
                mCoGuestState.lockVideoUserList.getValue().add(seatInfo.userId);
            } else {
                mCoGuestState.lockVideoUserList.getValue().remove(seatInfo.userId);
            }
            mCoGuestState.lockVideoUserList.setValue(mCoGuestState.lockVideoUserList.getValue());
            updateSelfMediaDeviceState(seatInfo);
        }
    }

    private void updateSelfMediaDeviceState(TUIRoomDefine.SeatInfo seatInfo) {
        if (seatInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(seatInfo.userId)) {
            return;
        }
        if (!seatInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        boolean isAudioLocked = seatInfo.isAudioLocked;
        boolean isVideoLocked = seatInfo.isVideoLocked;
        if (isAudioLocked != mMediaState.isAudioLocked.getValue()) {
            mMediaState.isAudioLocked.setValue(isAudioLocked);
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(isAudioLocked ? R.string.live_mute_audio_by_master :
                            R.string.live_un_mute_audio_by_master));
        }
        if (isVideoLocked != mMediaState.isVideoLocked.getValue()) {
            mMediaState.isVideoLocked.setValue(isVideoLocked);
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(isVideoLocked ? R.string.live_mute_video_by_owner :
                            R.string.live_un_mute_video_by_master));
        }
    }

    public void onRequestCancelled(UserInfo inviter) {
        mCoGuestState.coGuestStatus.setValue(NONE);
    }

    public void onKickedOffSeat() {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.live_voiceroom_kicked_out_of_seat));
    }

    public void onUserConnectionAccepted(String userId) {
        if (!mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
            mCoGuestState.coGuestStatus.setValue(LINKING);
        }
    }

    public void onUserConnectionRejected(String userId) {
        mCoGuestState.coGuestStatus.setValue(NONE);
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.live_voiceroom_take_seat_rejected));
    }

    public void onUserConnectionTimeout(String userId) {
        mCoGuestState.coGuestStatus.setValue(NONE);
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.live_voiceroom_take_seat_timeout));
    }
}
