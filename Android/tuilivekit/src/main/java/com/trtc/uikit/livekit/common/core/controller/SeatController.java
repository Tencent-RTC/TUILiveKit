package com.trtc.uikit.livekit.common.core.controller;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.error.ErrorHandler;
import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.core.store.state.operation.SeatState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.ToastUtils;

import java.util.List;

public class SeatController extends Controller {
    private static final String TAG = "SeatController";

    public SeatController(LiveState state, RoomEngineService service) {
        super(state, service);
        mRoomEngineService.addObserver(mObserver);
    }

    @Override
    public void destroy() {
        mRoomEngineService.removeObserver(mObserver);
    }

    public void getSeatList() {
        LiveKitLog.info(TAG + " getSeatList start");
        mRoomEngineService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                mSeatState.initSeatList(list);
                LiveKitLog.info(TAG + " getSeatList:Success:" + mSeatState.toString());
                updateSelfSeatedState();
                autoTakeSeatByOwner();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " getSeatList:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void takeSeat(int index) {
        LiveKitLog.info(TAG + " takeSeat start");
        TUIRoomDefine.Request request = mRoomEngineService.takeSeat(index, 60, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                LiveKitLog.info(TAG + " takeSeat:onAccepted, [requestId:" + requestId + ",userId:" + userId + "]");
                mSeatState.mySeatApplicationId.set("", false);
                ToastUtils.toast(R.string.livekit_voiceroom_take_seat_success);
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                LiveKitLog.info(TAG + " takeSeat:onRejected, [requestId:" + requestId + ",userId:" + userId + "]");
                mSeatState.mySeatApplicationId.set("", false);
                ToastUtils.toast(R.string.livekit_voiceroom_take_seat_rejected);
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                LiveKitLog.info(TAG + " takeSeat:onCancelled, [requestId:" + requestId + ",userId:" + userId + "]");
                mSeatState.mySeatApplicationId.set("", false);
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                LiveKitLog.info(TAG + " takeSeat:onTimeout, [requestId:" + requestId + ",userId:" + userId + "]");
                mSeatState.mySeatApplicationId.set("", false);
                ToastUtils.toast(R.string.livekit_voiceroom_take_seat_timeout);
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                LiveKitLog.info(TAG + " takeSeat:onError, [requestId:" + requestId + ",userId:" + userId + ",error:"
                        + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
                mSeatState.mySeatApplicationId.set("", false);
            }
        });
        mSeatState.mySeatApplicationId.set(request.requestId, false);
    }

    public void leaveSeat() {
        LiveKitLog.info(TAG + " leaveSeat start");
        mRoomEngineService.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " leaveSeat success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " leaveSeat:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void lockSeat(SeatState.SeatInfo seatInfo) {
        LiveKitLog.info(TAG + " lockSeat start");
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = seatInfo.isAudioLocked.get();
        params.lockSeat = !seatInfo.isLocked.get();
        mRoomEngineService.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " lockSeat success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " lockSeat:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void muteSeatAudio(SeatState.SeatInfo seatInfo) {
        LiveKitLog.info(TAG + " muteSeatAudio start");
        TUIRoomDefine.SeatLockParams params = new TUIRoomDefine.SeatLockParams();
        params.lockAudio = !seatInfo.isAudioLocked.get();
        params.lockSeat = seatInfo.isLocked.get();
        mRoomEngineService.lockSeat(seatInfo.index, params, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " muteSeatAudio success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " muteSeatAudio:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void kickSeat(String userId) {
        LiveKitLog.info(TAG + " kickSeat start");
        mRoomEngineService.kickSeat(userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " kickSeat success");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " kickSeat:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void getSeatApplicationList() {
        LiveKitLog.info(TAG + " getSeatApplicationList start");
        mRoomEngineService.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                LiveKitLog.info(TAG + " getSeatApplicationList success, size:" + list.size());
                mSeatState.initSeatApplicationList(list);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " kickSeat:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void acceptRequest(String requestId) {
        mRoomEngineService.acceptRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " acceptRequest success");
                mSeatState.removeSeatApplication(requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " acceptRequest:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void rejectRequest(String requestId) {
        mRoomEngineService.rejectRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " rejectRequest success");
                mSeatState.removeSeatApplication(requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " rejectRequest:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    public void cancelTakeSeatApplication() {
        String requestId = mSeatState.mySeatApplicationId.get();
        if (TextUtils.isEmpty(mSeatState.mySeatApplicationId.get())) {
            LiveKitLog.info(TAG + " cancelTakeSeatApplication mySeatApplicationId is empty");
            return;
        }
        mRoomEngineService.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " cancelRequest success");
                mSeatState.removeSeatApplication(requestId);
                mSeatState.mySeatApplicationId.set("");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " cancelRequest:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    private void updateSelfSeatedState() {
        if (isInSeat()) {
            mUserState.selfInfo.isInSeat.set(true);
        } else {
            mUserState.selfInfo.isInSeat.set(false);
        }
    }

    private void autoTakeSeatByOwner() {
        if (mUserState.selfInfo.role.get() != TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (!(mUserState.selfInfo.isInSeat.get())) {
            takeSeat(0);
        }
    }

    private boolean isInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        for (SeatState.SeatInfo seatInfo : mSeatState.seatList.get()) {
            if (selfUserId.equals(seatInfo.userId.get())) {
                return true;
            }
        }
        return false;
    }

    private final TUIRoomObserver mObserver = new TUIRoomObserver() {
        @Override
        public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                      List<TUIRoomDefine.SeatInfo> leftList) {

            mSeatState.updateSeatList(seatList);
            LiveKitLog.info(TAG + " onSeatInfoChanged:" + mSeatState.toString());
            updateSelfSeatedState();

            for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
                LiveKitLog.info(TAG + " onUserSeated: [user:" + seatInfo.userId + "]");
            }

            for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
                LiveKitLog.info(TAG + " onUserLeft: [user:" + seatInfo.userId + "]");
            }
        }

        @Override
        public void onRequestReceived(TUIRoomDefine.Request request) {
            if (request.requestAction != TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
                return;
            }
            LiveKitLog.info(TAG + " onRequestReceived takeSeat,id:" + request.requestId + ",userId:" + request.userId);
            mSeatState.addSeatApplication(request);
        }

        @Override
        public void onRequestCancelled(String requestId, String userId) {
            LiveKitLog.info(TAG + " onRequestCancelled:" + requestId + ",userId:" + userId);
            mSeatState.removeSeatApplication(requestId);
        }

        @Override
        public void onRequestProcessed(String requestId, String userId) {
            LiveKitLog.info(TAG + " onRequestProcessed:" + requestId + ",userId:" + userId);
            mSeatState.removeSeatApplication(requestId);
        }

        @Override
        public void onKickedOffSeat(String userId) {
            LiveKitLog.info(TAG + " onKickedOffSeat:" + userId);
            ToastUtils.toast(R.string.livekit_voiceroom_kicked_out_of_seat);
        }
    };
}
