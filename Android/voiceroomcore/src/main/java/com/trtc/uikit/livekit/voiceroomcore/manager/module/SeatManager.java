package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.List;

public class SeatManager extends BaseManager {
    private static final String FILE = "SeatManager";

    public SeatManager(VoiceRoomState state, IVoiceRoomService service, SeatGridViewObserverManager observerManager) {
        super(state, service, observerManager);
    }

    @Override
    public void destroy() {

    }

    public void getSeatList() {
        mService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                updateSeatInfo(list);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getSeatList, error: " + error + ", message: " + message);
            }
        });
    }

    public void getSeatApplicationList() {
        mService.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                initSeatApplicationList(list);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getSeatApplicationList, error: " + error + ", message: " + message);
            }
        });
    }

    public void setSeatList(VoiceRoomDefine.SeatViewLayoutConfig layoutConfig, int seatCount) {
        if (seatCount > 0) {
            updateSeatList(layoutConfig, seatCount);
        }
    }

    public void takeSeat(int index, int timeout, VoiceRoomDefine.RequestCallback callback) {
        TUIRoomDefine.Request request = mService.takeSeat(index, timeout, new TUIRoomDefine.RequestCallback() {
            @Override
            public void onAccepted(String requestId, String userId) {
                mSeatState.mySeatApplicationId = "";
                getUserInfo(userId, userInfo -> {
                    if (callback != null) {
                        callback.onAccepted(userInfo);
                    }
                });
            }

            @Override
            public void onRejected(String requestId, String userId, String message) {
                mSeatState.mySeatApplicationId = "";
                getUserInfo(userId, userInfo -> {
                    if (callback != null) {
                        callback.onRejected(userInfo);
                    }
                });
            }

            @Override
            public void onCancelled(String requestId, String userId) {
                mSeatState.mySeatApplicationId = "";
                getUserInfo(userId, userInfo -> {
                    if (callback != null) {
                        callback.onCancelled(userInfo);
                    }
                });
            }

            @Override
            public void onTimeout(String requestId, String userId) {
                mSeatState.mySeatApplicationId = "";
                getUserInfo(userId, userInfo -> {
                    if (callback != null) {
                        callback.onTimeout(userInfo);
                    }
                });
            }

            @Override
            public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "takeSeat, error: " + error + ", message: " + message);
                mSeatState.mySeatApplicationId = "";
                getUserInfo(userId, user -> {
                    if (callback != null) {
                        callback.onError(user, error, message);
                    }
                });
            }
        });
        mSeatState.mySeatApplicationId = request.requestId;
    }

    public void moveToSeat(int index, TUIRoomDefine.ActionCallback callback) {
        mService.moveToSeat(index, callback);
    }

    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        mService.leaveSeat(callback);
    }

    public void takeUserOnSeatByAdmin(int index, String user, int timeout, VoiceRoomDefine.RequestCallback callback) {
        TUIRoomDefine.Request request = mService.takeUserOnSeatByAdmin(index, user, timeout,
                new TUIRoomDefine.RequestCallback() {
                    @Override
                    public void onAccepted(String requestId, String userId) {
                        mSeatState.sentSeatInvitationMap.remove(userId);
                        getUserInfo(userId, user -> {
                            if (callback != null) {
                                callback.onAccepted(user);
                            }
                        });
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        mSeatState.sentSeatInvitationMap.remove(userId);
                        getUserInfo(userId, user -> {
                            if (callback != null) {
                                callback.onRejected(user);
                            }
                        });
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {
                        mSeatState.sentSeatInvitationMap.remove(userId);
                        getUserInfo(userId, user -> {
                            if (callback != null) {
                                callback.onCancelled(user);
                            }
                        });
                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        mSeatState.sentSeatInvitationMap.remove(userId);
                        getUserInfo(userId, user -> {
                            if (callback != null) {
                                callback.onTimeout(user);
                            }
                        });
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                        Logger.error(FILE, "takeUserOnSeatByAdmin, error: " + error + ", message: " + message);
                        mSeatState.sentSeatInvitationMap.remove(userId);
                        getUserInfo(userId, user -> {
                            if (callback != null) {
                                callback.onError(user, error, message);
                            }
                        });
                    }
                });
        addSendSeatInvitation(request);
    }

    public void kickUserOffSeatByAdmin(String userId, TUIRoomDefine.ActionCallback callback) {
        mService.kickUserOffSeatByAdmin(userId, callback);
    }

    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams params,
                         TUIRoomDefine.ActionCallback callback) {
        mService.lockSeat(seatIndex, params, callback);
    }

    public void cancelRequest(String userId, TUIRoomDefine.ActionCallback callback) {
        if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.GENERAL_USER) {
            cancelApplication(callback);
        } else {
            cancelInvitation(userId, callback);
        }
    }

    public void responseRemoteRequest(String userId, boolean agree, TUIRoomDefine.ActionCallback callback) {
        if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.GENERAL_USER) {
            responseInvitation(agree, callback);
        } else {
            responseApplication(userId, agree, callback);
        }
    }

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        updateSeatInfo(seatList);
        if (!leftList.isEmpty()) {
            for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
                mSeatState.seatUserMap.remove(seatInfo.userId);
            }
        }
    }

    public void onRequestReceived(TUIRoomDefine.Request request) {
        switch (request.requestAction) {
            case REQUEST_TO_TAKE_SEAT:
                onReceivedTakeSeat(request);
                break;
            case REQUEST_REMOTE_USER_ON_SEAT:
                addReceivedSeatInvitation(request);
                onReceivedInviteSeat(request);
                break;
            default:
                break;
        }
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        switch (request.requestAction) {
            case REQUEST_TO_TAKE_SEAT:
                onTakeSeatCancelled(VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT, request);
                break;
            case REQUEST_REMOTE_USER_ON_SEAT:
                onInviteSeatCancelled(VoiceRoomDefine.RequestType.INVITE_TO_TAKE_SEAT, request);
                break;
            default:
                break;
        }
    }

    public void onRequestProcessed(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        switch (request.requestAction) {
            case REQUEST_TO_TAKE_SEAT:
                onTakeSeatCancelled(VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT, request);
                break;
            case REQUEST_REMOTE_USER_ON_SEAT:
                onInviteSeatCancelled(VoiceRoomDefine.RequestType.INVITE_TO_TAKE_SEAT, request);
                break;
            default:
                break;
        }
    }

    public void onKickedOffSeat(int seatIndex, TUIRoomDefine.UserInfo operateUser) {
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onKickedOffSeat(operateUser);
        }
    }

    /*************************************************************************************/

    private void cancelApplication(TUIRoomDefine.ActionCallback callback) {
        if (TextUtils.isEmpty(mSeatState.mySeatApplicationId)) {
            Logger.error(FILE, "cancelApplication error, mySeatApplicationId is empty");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat application");
            }
            return;
        }
        mService.cancelRequest(mSeatState.mySeatApplicationId, callback);
        mSeatState.mySeatApplicationId = "";
    }

    private void cancelInvitation(String userId, TUIRoomDefine.ActionCallback callback) {
        TUIRoomDefine.Request request = mSeatState.sentSeatInvitationMap.get(userId);
        if (request == null || TextUtils.isEmpty(request.requestId)) {
            Logger.error(FILE, "cancelInvitation error, request id is empty");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat invitation");
            }
            return;
        }
        mService.cancelRequest(request.requestId, callback);
        mSeatState.sentSeatInvitationMap.remove(userId);
    }

    private void responseApplication(String userId, boolean agree, TUIRoomDefine.ActionCallback callback) {
        TUIRoomDefine.Request request = mSeatState.seatApplicationMap.get(userId);
        if (request == null || TextUtils.isEmpty(request.requestId)) {
            Logger.error(FILE, "responseApplication error, request id is empty");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat application");
            }
            return;
        }
        if (agree) {
            mService.acceptRequest(request.requestId, callback);
        } else {
            mService.rejectRequest(request.requestId, callback);
        }
        mSeatState.seatApplicationMap.remove(userId);
    }

    private void responseInvitation(boolean agree, TUIRoomDefine.ActionCallback callback) {
        String requestId = mSeatState.receivedSeatInvitation.requestId;
        if (TextUtils.isEmpty(requestId)) {
            Logger.error(FILE, "responseInvitation error, request id is empty");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat invitation");
            }
            return;
        }
        if (agree) {
            mService.acceptRequest(requestId, callback);
        } else {
            mService.rejectRequest(requestId, callback);
        }
        mSeatState.receivedSeatInvitation.requestId = "";
        mSeatState.receivedSeatInvitation.userId = "";
    }

    private void onReceivedTakeSeat(TUIRoomDefine.Request request) {
        TUIRoomDefine.UserInfo userInfo = convertToUserInfo(request);
        addSeatApplication(request);
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onSeatRequestReceived(VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT, userInfo);
        }
    }

    private void onReceivedInviteSeat(TUIRoomDefine.Request request) {
        TUIRoomDefine.UserInfo userInfo = convertToUserInfo(request);
        addReceivedSeatInvitation(request);
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onSeatRequestReceived(VoiceRoomDefine.RequestType.INVITE_TO_TAKE_SEAT, userInfo);
        }
    }

    private void onTakeSeatCancelled(VoiceRoomDefine.RequestType type, TUIRoomDefine.Request request) {
        TUIRoomDefine.UserInfo userInfo = convertToUserInfo(request);
        mSeatState.seatApplicationMap.remove(request.userId);
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onSeatRequestCancelled(type, userInfo);
        }
    }

    private void onInviteSeatCancelled(VoiceRoomDefine.RequestType type, TUIRoomDefine.Request request) {
        TUIRoomDefine.UserInfo userInfo = convertToUserInfo(request);
        mSeatState.receivedSeatInvitation.requestId = "";
        mSeatState.receivedSeatInvitation.userId = "";
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onSeatRequestCancelled(type, userInfo);
        }
    }

    private TUIRoomDefine.UserInfo convertToUserInfo(TUIRoomDefine.Request request) {
        TUIRoomDefine.UserInfo userInfo = new TUIRoomDefine.UserInfo();
        userInfo.userId = request.userId;
        userInfo.userName = request.userName;
        userInfo.avatarUrl = request.avatarUrl;
        return userInfo;
    }

    private void updateSeatInfo(List<TUIRoomDefine.SeatInfo> list) {
        for (int i = 0; i < mSeatState.seatList.size(); i++) {
            if (i >= list.size()) {
                break;
            }
            SeatState.Seat oldSeatInfo = mSeatState.seatList.getList().get(i);
            TUIRoomDefine.SeatInfo newSeatInfo = list.get(i);
            boolean isChanged = isSeatInfoChanged(newSeatInfo, oldSeatInfo);
            if (isChanged) {
                oldSeatInfo.seatInfo = newSeatInfo;
                SeatState.Seat seat = oldSeatInfo.update(newSeatInfo);
                updateSeatInfoMap(seat);
                mSeatState.seatList.change(oldSeatInfo);
            }
        }
    }

    private void addSeatApplication(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mSeatState.seatApplicationMap.put(request.userId, request);
    }

    private void addSendSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mSeatState.sentSeatInvitationMap.put(request.userId, request);
    }

    private void addReceivedSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mSeatState.receivedSeatInvitation = request;
    }

    private boolean isRequestInvalid(TUIRoomDefine.Request request) {
        if (request == null) {
            return true;
        }
        if (TextUtils.isEmpty(request.userId)) {
            return true;
        }
        return TextUtils.isEmpty(request.requestId);
    }

    private void updateSeatList(VoiceRoomDefine.SeatViewLayoutConfig layoutConfig, int seatCount) {
        int seatIndex = 0;
        int rowIndex = 0;
        int columnIndex = 0;
        List<SeatState.Seat> list = new ArrayList<>();
        while (rowIndex < layoutConfig.rowConfigs.size()) {
            columnIndex = 0;
            VoiceRoomDefine.SeatViewLayoutRowConfig rowConfig = layoutConfig.rowConfigs.get(rowIndex);
            while (columnIndex < rowConfig.count) {
                if (seatIndex >= seatCount) {
                    break;
                }
                SeatState.Seat seat = getSeatInfoFromIndex(seatIndex);
                if (seat == null) {
                    seat = new SeatState.Seat();
                    seat.seatInfo = new TUIRoomDefine.SeatInfo();
                    seat.seatInfo.index = seatIndex;
                    mSeatState.seatList.getList().add(seat);
                }
                seat.rowIndex = rowIndex;
                seat.columnIndex = columnIndex;
                list.add(seat);
                seatIndex++;
                columnIndex++;
            }
            rowIndex++;
        }
        addExtraSeatInfo(seatIndex, seatCount, list, rowIndex, columnIndex);
        mSeatState.seatList.replaceAll(list);
    }

    private void addExtraSeatInfo(int seatIndex, int seatCount, List<SeatState.Seat> list, int rowIndex,
                                  int columnIndex) {
        while (seatIndex < seatCount) {
            SeatState.Seat seat = new SeatState.Seat();
            seat.rowIndex = rowIndex;
            seat.columnIndex = columnIndex;
            seat.seatInfo = new TUIRoomDefine.SeatInfo();
            seat.seatInfo.index = seatIndex;
            list.add(seat);
            columnIndex++;
            seatIndex++;
        }
        mSeatState.seatList.redirect(list);
    }

    private SeatState.Seat getSeatInfoFromIndex(int seatIndex) {
        if (seatIndex >= mSeatState.seatList.size()) {
            return null;
        }
        return mSeatState.seatList.get(seatIndex);
    }

    private boolean isSeatInfoChanged(TUIRoomDefine.SeatInfo newSeatInfo, SeatState.Seat oldSeat) {
        if (oldSeat == null || newSeatInfo == null) {
            return false;
        }
        TUIRoomDefine.SeatInfo oldSeatInfo = oldSeat.seatInfo;
        if (oldSeatInfo == null) {
            return true;
        }

        if (newSeatInfo.isLocked != oldSeatInfo.isLocked) {
            return true;
        }

        if (newSeatInfo.isAudioLocked != oldSeatInfo.isAudioLocked) {
            return true;
        }
        return !newSeatInfo.userId.equals(oldSeatInfo.userId);
    }

    private void updateSeatInfoMap(SeatState.Seat seat) {
        if (seat == null) {
            return;
        }
        TUIRoomDefine.SeatInfo seatInfo = seat.seatInfo;
        if (seatInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(seatInfo.userId)) {
            return;
        }
        mSeatState.seatUserMap.put(seatInfo.userId, seat);
    }

    private void initSeatApplicationList(List<TUIRoomDefine.Request> list) {
        mSeatState.seatApplicationMap.clear();
        for (TUIRoomDefine.Request request : list) {
            addSeatApplication(request);
            TUIRoomDefine.UserInfo userInfo = convertToUserInfo(request);
            for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
                observer.onSeatRequestReceived(VoiceRoomDefine.RequestType.APPLY_TO_TAKE_SEAT, userInfo);
            }
        }
    }
}
