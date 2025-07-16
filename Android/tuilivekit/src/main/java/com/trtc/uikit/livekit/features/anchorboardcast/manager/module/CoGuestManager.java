package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CoGuestManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("CoGuestManager");

    private final Map<String, TUIRoomDefine.SeatInfo> seatInfoMap = new HashMap<>();

    public CoGuestManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    public enum MediaDevice {
        MICROPHONE,
        CAMERA
    }

    @Override
    public void destroy() {
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
                LOGGER.info("lockSeat:[success]");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("lockSeat failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    /******************************************  Observer *******************************************/
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
        if (!seatInfo.userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
            return;
        }
        boolean isAudioLocked = seatInfo.isAudioLocked;
        boolean isVideoLocked = seatInfo.isVideoLocked;
        if (isAudioLocked != mMediaState.isAudioLocked.getValue()) {
            mMediaState.isAudioLocked.setValue(isAudioLocked);
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(isAudioLocked ? R.string.common_mute_audio_by_master :
                            R.string.common_un_mute_audio_by_master));
        }
        if (isVideoLocked != mMediaState.isVideoLocked.getValue()) {
            mMediaState.isVideoLocked.setValue(isVideoLocked);
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(isVideoLocked ? R.string.common_mute_video_by_owner :
                            R.string.common_un_mute_video_by_master));
        }
    }

    public void onRequestCancelled(UserInfo inviter) {

    }

    public void onKickedOffSeat() {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_voiceroom_kicked_out_of_seat));
    }

    public void onUserConnectionAccepted(String userId) {

    }

    public void onUserConnectionRejected(String userId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_voiceroom_take_seat_rejected));
    }

    public void onUserConnectionTimeout(String userId) {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_voiceroom_take_seat_timeout));
    }
}
