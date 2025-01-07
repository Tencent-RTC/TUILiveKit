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
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.ArrayList;
import java.util.List;

public class CoGuestManager extends BaseManager {
    private static final String TAG = "CoGuestManager";

    public CoGuestManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public void enableAutoOpenCameraOnSeated(boolean enable) {
        mCoGuestState.openCameraOnCoGuest = enable;
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
                mCoGuestState.connectedUserList.notifyDataChanged();
                updateSelfSeatedState();
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
                List<UserInfo> userList = new ArrayList<>();
                for (TUIRoomDefine.Request request : list) {
                    UserInfo liveUser = new UserInfo();
                    liveUser.userId = request.userId;
                    liveUser.userName = request.userName;
                    liveUser.avatarUrl = request.avatarUrl;
                    userList.add(liveUser);
                }
                initSeatApplicationList(userList);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorHandler.onError(error);
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
        mCoGuestState.connectedUserList.get().clear();
        mCoGuestState.connectedUserList.addAll(newList);
    }

    private void initSeatApplicationList(List<UserInfo> list) {
        mCoGuestState.requestCoGuestList.get().clear();
        List<CoGuestState.SeatApplication> newList = new ArrayList<>();
        for (UserInfo liveUser : list) {
            CoGuestState.SeatApplication application = new CoGuestState.SeatApplication(liveUser.userId);
            application.updateState(liveUser);
            newList.add(application);
        }
        mCoGuestState.requestCoGuestList.addAll(newList);
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mCoGuestState.coGuestStatus.set(LINKING, false);
        }
    }


    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mCoGuestState.connectedUserList.get().contains(new CoGuestState.SeatInfo(selfUserId));
    }

    private boolean isSelfSeatInfo(UserInfo seatInfo) {
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mUserState.selfInfo.userId.equals(seatInfo.userId);
    }

    private void addSeatApplication(UserInfo inviter) {
        CoGuestState.SeatApplication seatApplication = new CoGuestState.SeatApplication(inviter.userId);
        seatApplication.updateState(inviter);
        mCoGuestState.requestCoGuestList.add(seatApplication);
    }

    public void removeSeatApplication(String userId) {
        CoGuestState.SeatApplication application = new CoGuestState.SeatApplication(userId);
        mCoGuestState.requestCoGuestList.remove(application);
    }

    public void updateCoGuestStates(CoGuestState.CoGuestStatus linkStatus) {
        mCoGuestState.coGuestStatus.set(linkStatus, false);
    }

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<UserInfo> userList, List<UserInfo> joinList,
                                  List<UserInfo> leaveList) {
        initSeatList(userList);
        for (UserInfo seatInfo : joinList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.set(LINKING, false);
            }
        }
        if (!joinList.isEmpty()) {
            mCoGuestState.connectedUserList.notifyDataChanged();
        }
        for (UserInfo seatInfo : leaveList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.set(NONE, false);
            }
        }
        if (!leaveList.isEmpty()) {
            mCoGuestState.connectedUserList.notifyDataChanged();
        }
    }

    public void onRequestReceived(UserInfo inviter) {
        addSeatApplication(inviter);
    }

    public void onRequestCancelled(UserInfo inviter) {
        removeSeatApplication(inviter.userId);

        mCoGuestState.coGuestStatus.set(NONE, false);
    }

    public void onKickedOffSeat() {
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.livekit_voiceroom_kicked_out_of_seat));
    }

    public void onUserConnectionAccepted(String userId) {
        if (!mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
            mCoGuestState.coGuestStatus.set(LINKING, false);
        }
    }

    public void onUserConnectionRejected(String userId) {
        mCoGuestState.coGuestStatus.set(NONE, false);
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.livekit_voiceroom_take_seat_rejected));
    }

    public void onUserConnectionTimeout(String userId) {
        mCoGuestState.coGuestStatus.set(NONE, false);
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.livekit_voiceroom_take_seat_timeout));
    }
}
