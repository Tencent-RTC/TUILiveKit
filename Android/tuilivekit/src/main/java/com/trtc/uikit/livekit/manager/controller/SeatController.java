package com.trtc.uikit.livekit.manager.controller;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.ToastUtils;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.List;

public class SeatController extends Controller {
    private static final String TAG = "SeatController";

    public SeatController(LiveState state, ILiveService service) {
        super(state, service);
    }

    @Override
    public void destroy() {
        LiveKitLog.info(TAG + " destroy");
    }

    public void getSeatList() {
        mLiveService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                mSeatState.updateSeatList(list);
                updateSelfSeatedState();
                autoTakeSeatByOwner();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void takeSeat(int index) {
        if (needRequestToTakeSeat()) {
            mViewState.linkStatus.set(LiveDefine.LinkStatus.APPLYING);
        }
        TUIRoomDefine.Request request = mLiveService.takeSeat(index, 60, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                mSeatState.mySeatApplicationId.set("", false);
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                mSeatState.mySeatApplicationId.set("", false);
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
                ToastUtils.toast(R.string.livekit_voiceroom_take_seat_rejected);
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                mSeatState.mySeatApplicationId.set("", false);
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                mSeatState.mySeatApplicationId.set("", false);
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
                ToastUtils.toast(R.string.livekit_voiceroom_take_seat_timeout);
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
                mSeatState.mySeatApplicationId.set("", false);
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
            }
        });
        mSeatState.mySeatApplicationId.set(request.requestId, false);
    }

    public void leaveSeat() {
        mLiveService.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void lockSeat(SeatState.SeatInfo seatInfo) {
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = seatInfo.isAudioLocked.get();
        params.lockSeat = !seatInfo.isLocked.get();
        mLiveService.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void muteSeatAudio(SeatState.SeatInfo seatInfo) {
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = !seatInfo.isAudioLocked.get();
        params.lockSeat = seatInfo.isLocked.get();
        mLiveService.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void kickSeat(String userId) {
        mLiveService.kickSeat(userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void getSeatApplicationList() {
        mLiveService.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                mSeatState.initSeatApplicationList(list);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void acceptRequest(String requestId) {
        mLiveService.acceptRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mSeatState.removeSeatApplication(requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void rejectRequest(String requestId) {
        mLiveService.rejectRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mSeatState.removeSeatApplication(requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void cancelTakeSeatApplication() {
        String requestId = mSeatState.mySeatApplicationId.get();
        if (TextUtils.isEmpty(mSeatState.mySeatApplicationId.get())) {
            LiveKitLog.error(TAG + " cancelTakeSeatApplication mySeatApplicationId is empty");
            return;
        }
        mLiveService.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mSeatState.removeSeatApplication(requestId);
                mSeatState.mySeatApplicationId.set("");
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    public void kickUserOffSeatByAdmin(int seatIndex, SeatState.SeatInfo seatInfo,
                                       TUIRoomDefine.ActionCallback callback) {
        mLiveService.kickUserOffSeatByAdmin(seatIndex, seatInfo.userId.get(), new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mSeatState.seatList.remove(seatInfo);
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mViewState.linkStatus.set(LiveDefine.LinkStatus.LINKING, false);
        }
    }

    private void autoTakeSeatByOwner() {
        if (mUserState.selfInfo.role.get() != TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (mViewState.linkStatus.get() != LiveDefine.LinkStatus.LINKING) {
            takeSeat(0);
        }
    }

    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mSeatState.seatList.get().contains(new SeatState.SeatInfo(selfUserId));
    }

    private boolean needRequestToTakeSeat() {
        TUIRoomDefine.Role role = mUserState.selfInfo.role.get();
        if (role == TUIRoomDefine.Role.ROOM_OWNER || role == TUIRoomDefine.Role.MANAGER) {
            return false;
        }
        return mRoomState.seatMode.get() == TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
    }

    private boolean isSelfSeatInfo(TUIRoomDefine.SeatInfo seatInfo) {
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mUserState.selfInfo.userId.equals(seatInfo.userId);
    }


    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        mSeatState.updateSeatList(seatList);
        for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
            LiveKitLog.info(TAG + " onUserSeated: [user:" + seatInfo.userId + "]");
            if (isSelfSeatInfo(seatInfo)) {
                mViewState.linkStatus.set(LiveDefine.LinkStatus.LINKING, false);
            }
        }

        for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
            LiveKitLog.info(TAG + " onUserLeft: [user:" + seatInfo.userId + "]");
            if (isSelfSeatInfo(seatInfo)) {
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
            }
        }
    }

    public void onRequestReceived(TUIRoomDefine.Request request) {
        if (request.requestAction != TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
            return;
        }
        mSeatState.addSeatApplication(request);
    }

    public void onRequestCancelled(String requestId, String userId) {
        mSeatState.removeSeatApplication(requestId);
    }

    public void onRequestProcessed(String requestId, String userId) {
        mSeatState.removeSeatApplication(requestId);
    }

    public void onKickedOffSeat(String userId) {
        ToastUtils.toast(R.string.livekit_voiceroom_kicked_out_of_seat);
    }
}
