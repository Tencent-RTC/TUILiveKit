package com.trtc.uikit.livekit.manager.controller;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.ToastUtils;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.SeatState;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.List;
import java.util.Map;

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
                mSeatState.seatList.notifyDataChanged();
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
        if (mViewState.linkStatus.get() == LiveDefine.LinkStatus.LINKING) {
            return;
        }
        if (needRequestToTakeSeat()) {
            mViewState.linkStatus.set(LiveDefine.LinkStatus.APPLYING);
        }
        TUIRoomDefine.Request request = mLiveService.takeSeat(index, 60, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                mSeatState.mySeatApplicationId.set("", false);
                if (!mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
                    mViewState.linkStatus.set(LiveDefine.LinkStatus.LINKING, false);
                }
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

    public void takeUserOnSeatByAdmin(int index, UserState.UserInfo userInfo) {
        TUIRoomDefine.Request request = mLiveService.takeUserOnSeatByAdmin(index, userInfo.userId, 60,
                new TUIRoomDefine.RequestCallback() {
                    @Override
                    public void onAccepted(String requestId, String userId) {
                        mSeatState.removeSentSeatInvitation(userInfo.userId);
                        ToastUtils.toast(TUIConfig.getAppContext().getString(
                                R.string.livekit_voiceroom_invite_seat_success, userInfo.name.get()));
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        mSeatState.removeSentSeatInvitation(userInfo.userId);
                        ToastUtils.toast(TUIConfig.getAppContext().getString(
                                R.string.livekit_voiceroom_invite_seat_rejected, userInfo.name.get()));
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {
                        ToastUtils.toast(R.string.livekit_voiceroom_invite_seat_canceled);
                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        mSeatState.removeSentSeatInvitation(userInfo.userId);
                        ToastUtils.toast(R.string.livekit_voiceroom_invite_seat_timeout);
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                        mSeatState.removeSentSeatInvitation(userInfo.userId);
                        ErrorHandler.onError(error);
                    }
                });
        mSeatState.addSendSeatInvitation(request);
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
        if (seatInfo == null) {
            return;
        }
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

    public void responseSeatApplication(boolean isAgree, String requestId) {
        if (isAgree) {
            acceptRequest(requestId);
        } else {
            rejectRequest(requestId);
        }
        mSeatState.removeSeatApplication(requestId);
    }

    public void cancelSeatApplication() {
        String requestId = mSeatState.mySeatApplicationId.get();
        if (TextUtils.isEmpty(requestId)) {
            LiveKitLog.error(TAG + " cancelSeatApplication requestId is empty");
            return;
        }
        cancelRequest(requestId);
        mSeatState.mySeatApplicationId.set("");
        mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
    }

    public void responseSeatInvitation(boolean isAgree, String requestId) {
        if (isAgree) {
            acceptRequest(requestId);
        } else {
            rejectRequest(requestId);
        }
        mSeatState.removeReceivedSeatInvitation();
    }

    public void cancelSeatInvitation(String userId) {
        if (TextUtils.isEmpty(userId)) {
            LiveKitLog.error(TAG + " cancelSeatInvitation userId is empty");
            return;
        }
        Map<String, SeatState.SeatInvitation> seatInvitationMap = mSeatState.sentSeatInvitationMap.get();
        SeatState.SeatInvitation seatInvitation = seatInvitationMap.get(userId);
        if (seatInvitation != null) {
            cancelRequest(seatInvitation.id);
            mSeatState.removeSentSeatInvitation(userId);
        }
    }

    public void kickUserOffSeatByAdmin(SeatState.SeatInfo seatInfo) {
        if (seatInfo == null) {
            return;
        }
        mLiveService.kickUserOffSeatByAdmin(0, seatInfo.userId.get(), new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
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

    private void acceptRequest(String requestId) {
        mLiveService.acceptRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " acceptRequest success:" + requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void rejectRequest(String requestId) {
        mLiveService.rejectRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " rejectRequest success:" + requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    private void cancelRequest(String requestId) {
        if (TextUtils.isEmpty(requestId)) {
            LiveKitLog.error(TAG + " cancelRequest requestId is empty");
            return;
        }
        mLiveService.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " cancelRequest success:" + requestId);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
            }
        });
    }

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        mSeatState.updateSeatList(seatList);
        for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
            if (isSelfSeatInfo(seatInfo)) {
                mViewState.linkStatus.set(LiveDefine.LinkStatus.LINKING, false);
            }
        }
        if (!seatedList.isEmpty()) {
            mSeatState.seatList.notifyDataChanged();
        }
        for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
            if (isSelfSeatInfo(seatInfo)) {
                mViewState.linkStatus.set(LiveDefine.LinkStatus.NONE, false);
            }
        }
        if (!leftList.isEmpty()) {
            mSeatState.seatList.notifyDataChanged();
        }
    }

    public void onRequestReceived(TUIRoomDefine.Request request) {
        switch (request.requestAction) {
            case REQUEST_TO_TAKE_SEAT:
                mSeatState.addSeatApplication(request);
                break;
            case REQUEST_REMOTE_USER_ON_SEAT:
                mSeatState.addReceivedSeatInvitation(request);
                break;
            default:
                break;
        }
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        switch (request.requestAction) {
            case REQUEST_TO_TAKE_SEAT:
                mSeatState.removeSeatApplication(request.requestId);
                break;
            case REQUEST_REMOTE_USER_ON_SEAT:
                mSeatState.removeReceivedSeatInvitation();
                break;
            default:
                break;
        }
    }

    public void onRequestProcessed(String requestId, String userId) {
        mSeatState.removeSeatApplication(requestId);
    }

    public void onKickedOffSeat(String userId) {
        ToastUtils.toast(R.string.livekit_voiceroom_kicked_out_of_seat);
    }
}
