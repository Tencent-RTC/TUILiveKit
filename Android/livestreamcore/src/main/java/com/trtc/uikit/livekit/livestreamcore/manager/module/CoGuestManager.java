package com.trtc.uikit.livekit.livestreamcore.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;
import com.trtc.uikit.livekit.livestreamcore.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.state.UserState;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class CoGuestManager extends BaseManager {
    private static final String TAG                   = "CoGuestManager";
    private static final int    REQUEST_DEFAULT_INDEX = -1;
    private static final int    REQUEST_TIMEOUT       = 60;

    private final CoGuestState    mCoGuestState;
    private final RoomState       mRoomState;
    private final UserState       mUserState;
    private       CoGuestObserver mCoGuestObserver;

    public CoGuestManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        mRoomState = state.roomState;
        mCoGuestState = state.coGuestState;
        mUserState = state.userState;
    }

    public void setCoGuestObserver(CoGuestObserver observer) {
        mCoGuestObserver = observer;
    }

    public boolean isEnable() {
        return mVideoLiveState.coGuestState.enableConnection;
    }

    public void setEnableConnection(boolean enable) {
        mVideoLiveState.coGuestState.enableConnection = enable;
    }

    @Override
    public void destroy() {
        Logger.info(TAG + " destroy");
    }

    public void initConnectedGuestList() {
        mVideoLiveService.getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                updateSelfSeatedState();
                autoTakeSeatByOwner();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public void initGuestApplicationList() {
        mVideoLiveService.getSeatApplicationList(new TUIRoomDefine.RequestListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.Request> list) {
                initSeatApplicationList(list);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public TUIRoomDefine.Request inviteGuestToConnection(String userId, int timeout,
                                                         TUIRoomDefine.RequestCallback callback) {
        TUIRoomDefine.Request request = mVideoLiveService.takeUserOnSeatByAdmin(REQUEST_DEFAULT_INDEX, userId, timeout,
                new TUIRoomDefine.RequestCallback() {
                    @Override
                    public void onAccepted(String requestId, String userId) {
                        mCoGuestState.sentSeatInvitationMap.remove(userId);
                        if (callback != null) {
                            callback.onAccepted(requestId, userId);
                        }
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        mCoGuestState.sentSeatInvitationMap.remove(userId);
                        if (callback != null) {
                            callback.onRejected(requestId, userId, message);
                        }
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {
                        mCoGuestState.sentSeatInvitationMap.remove(userId);
                        if (callback != null) {
                            callback.onCancelled(requestId, userId);
                        }
                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        mCoGuestState.sentSeatInvitationMap.remove(userId);
                        if (callback != null) {
                            callback.onTimeout(requestId, userId);
                        }
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                        mCoGuestState.sentSeatInvitationMap.remove(userId);
                        if (callback != null) {
                            callback.onError(requestId, userId, error, message);
                        }
                    }
                });
        addSendSeatInvitation(request);
        return request;
    }

    public void cancelGuestInvitation(String userId, TUIRoomDefine.ActionCallback callback) {
        TUIRoomDefine.Request request = mCoGuestState.sentSeatInvitationMap.get(userId);
        if (request == null || TextUtils.isEmpty(request.requestId)) {
            Logger.error("cancelInvitation error, request id is empty");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat invitation");
            }
            return;
        }
        mVideoLiveService.cancelRequest(request.requestId, callback);
        mCoGuestState.sentSeatInvitationMap.remove(userId);
    }

    public void respondGuestInvitation(TUIRoomDefine.UserInfo userInfo, boolean isAgree,
                                       TUIRoomDefine.ActionCallback callback) {
        String requestId = mCoGuestState.receivedSeatInvitation.requestId;
        if (TextUtils.isEmpty(requestId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "you don't have the seat invitation");
            }
            return;
        }
        if (isAgree) {
            mVideoLiveService.acceptRequest(requestId, callback);
        } else {
            mVideoLiveService.rejectRequest(requestId, callback);
        }
        mCoGuestState.receivedSeatInvitation.requestId = "";
        mCoGuestState.receivedSeatInvitation.userId = "";
    }

    public TUIRoomDefine.Request applyToConnection(int timeout, TUIRoomDefine.RequestCallback callback) {
        if (mCoGuestState.coGuestStatus.get() == CoGuestStatus.LINKING) {
            return null;
        }
        mCoGuestState.coGuestStatus.set(CoGuestStatus.APPLYING);
        TUIRoomDefine.Request request = mVideoLiveService.takeSeat(REQUEST_DEFAULT_INDEX, timeout,
                new TUIRoomDefine.RequestCallback() {
                    @Override
                    public void onAccepted(String requestId, String userId) {
                        mCoGuestState.myRequestId.set("", false);
                        if (!mRoomState.ownerInfo.userId.equals(mUserState.selfInfo.userId)) {
                            mCoGuestState.coGuestStatus.set(CoGuestStatus.LINKING, false);
                        }
                        if (callback != null) {
                            callback.onAccepted(requestId, userId);
                        }
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        mCoGuestState.myRequestId.set("", false);
                        mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
                        if (callback != null) {
                            callback.onRejected(requestId, userId, message);
                        }
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {
                        mCoGuestState.myRequestId.set("", false);
                        mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
                        if (callback != null) {
                            callback.onCancelled(requestId, userId);
                        }
                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        mCoGuestState.myRequestId.set("", false);
                        mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
                        if (callback != null) {
                            callback.onTimeout(requestId, userId);
                        }
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                        mCoGuestState.myRequestId.set("", false);
                        mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
                        if (callback != null) {
                            callback.onError(requestId, userId, error, message);
                        }
                    }
                });
        mCoGuestState.myRequestId.set(request.requestId, false);
        return request;
    }

    public void respondGuestApplication(String userId, boolean isAgree, TUIRoomDefine.ActionCallback callback) {
        if (TextUtils.isEmpty(userId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "UserId is empty");
            }
            return;
        }
        String requestId = getAudienceRequestIdByUserId(userId);
        if (TextUtils.isEmpty(requestId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "Invalid requestId");
            }
            return;
        }
        if (isAgree) {
            acceptRequest(requestId, callback);
        } else {
            rejectRequest(requestId, callback);
        }
        removeSeatApplication(requestId);
    }

    public void cancelGuestApplication(TUIRoomDefine.ActionCallback callback) {
        String requestId = mCoGuestState.myRequestId.get();
        if (TextUtils.isEmpty(requestId)) {
            Logger.error(TAG + " cancelSeatApplication requestId is empty");
            return;
        }
        cancelRequest(requestId, callback);
        mCoGuestState.myRequestId.set("");
        mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
    }

    public void disconnectBySelf() {
        mVideoLiveService.leaveSeat(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public void disconnectByAdmin(String userId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.kickUserOffSeatByAdmin(REQUEST_DEFAULT_INDEX, userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
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

    public void enableAutoOpenCameraOnSeated(boolean enable) {
        mCoGuestState.openCameraOnCoGuest = enable;
    }

    private String getAudienceRequestIdByUserId(String userId) {
        for (TUIRoomDefine.Request request : mCoGuestState.connectionRequestList.get()) {
            if (request.userId.equals(userId)) {
                return request.requestId;
            }
        }
        return "";
    }

    private void updateSelfSeatedState() {
        if (isSelfInSeat()) {
            mCoGuestState.coGuestStatus.set(CoGuestStatus.LINKING, false);
        }
    }

    private void autoTakeSeatByOwner() {
        if (mUserState.selfInfo.userRole != TUIRoomDefine.Role.ROOM_OWNER) {
            return;
        }
        if (mCoGuestState.coGuestStatus.get() != CoGuestStatus.LINKING) {
            applyToConnection(REQUEST_TIMEOUT, null);
        }
    }

    private boolean isSelfInSeat() {
        String selfUserId = mUserState.selfInfo.userId;
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        for (TUIRoomDefine.SeatInfo seatInfo : mCoGuestState.connectedUserList.get()) {
            if (selfUserId.equals(seatInfo.userId)) {
                return true;
            }
        }
        return false;
    }

    private void initSeatList(List<TUIRoomDefine.SeatInfo> list) {
        List<TUIRoomDefine.SeatInfo> newList = new ArrayList<>(list.size());
        for (TUIRoomDefine.SeatInfo info : list) {
            if (TextUtils.isEmpty(info.userId)) {
                continue;
            }
            newList.add(info);
        }
        mCoGuestState.connectedUserList.get().clear();
        mCoGuestState.connectedUserList.addAll(newList);
    }

    private void initSeatApplicationList(List<TUIRoomDefine.Request> list) {
        mCoGuestState.connectionRequestList.get().clear();
        mCoGuestState.connectionRequestList.addAll(list);
    }

    private void addSendSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mCoGuestState.sentSeatInvitationMap.put(request.userId, request);
    }

    private void addReceivedSeatInvitation(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mCoGuestState.receivedSeatInvitation = request;
    }

    private void addSeatApplication(TUIRoomDefine.Request request) {
        if (isRequestInvalid(request)) {
            return;
        }
        mCoGuestState.connectionRequestList.add(request);
    }

    private void removeSeatApplication(String id) {
        for (TUIRoomDefine.Request application : mCoGuestState.connectionRequestList.get()) {
            if (application.requestId.equals(id)) {
                mCoGuestState.connectionRequestList.remove(application);
                return;
            }
        }
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

    private boolean isSelfSeatInfo(TUIRoomDefine.SeatInfo seatInfo) {
        if (TextUtils.isEmpty(mUserState.selfInfo.userId)) {
            return false;
        }
        return mUserState.selfInfo.userId.equals(seatInfo.userId);
    }

    private void acceptRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.acceptRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(TAG + " acceptRequest success:" + requestId);
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

    private void rejectRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.rejectRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(TAG + " rejectRequest success:" + requestId);
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

    private void cancelRequest(String requestId, TUIRoomDefine.ActionCallback callback) {
        if (TextUtils.isEmpty(requestId)) {
            Logger.error(TAG + " cancelRequest requestId is empty");
            return;
        }
        mVideoLiveService.cancelRequest(requestId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                Logger.info(TAG + " cancelRequest success:" + requestId);
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

    /******************************************  Observer *******************************************/
    public void onSeatListChanged(List<TUIRoomDefine.SeatInfo> seatList, List<TUIRoomDefine.SeatInfo> seatedList,
                                  List<TUIRoomDefine.SeatInfo> leftList) {
        initSeatList(seatList);
        for (TUIRoomDefine.SeatInfo seatInfo : seatedList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.set(CoGuestStatus.LINKING, false);
            }
        }

        for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
            if (isSelfSeatInfo(seatInfo)) {
                mCoGuestState.coGuestStatus.set(CoGuestStatus.NONE, false);
            }
        }
        notifyConnectedUsersUpdated(seatList, seatedList, leftList);
        notifyUserConnectionExited(leftList);

    }

    public void onRequestReceived(TUIRoomDefine.Request request) {
        if (!mVideoLiveState.coHostState.connectedUserList.get().isEmpty() || !mVideoLiveState.coHostState.sentConnectionRequestList.get().isEmpty() || mVideoLiveState.coHostState.receivedConnectionRequest.get() != null) {
            rejectRequest(request.requestId, null);
            return;
        }

        if (Objects.requireNonNull(request.requestAction) == TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
            addSeatApplication(request);

            notifyUserConnectionRequest(request);
        }
    }

    public void onRequestCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        if (Objects.requireNonNull(request.requestAction) == TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT) {
            removeSeatApplication(request.requestId);
            notifyUserConnectionCancelled(request, operateUser);
        }
    }

    public void onRequestProcessed(String requestId, String userId) {
        removeSeatApplication(requestId);
    }

    public void onKickedOffSeat(String userId) {
        notifyConnectionTerminated(userId);
    }

    public void onConnectSuccess() {
        if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
            mVideoLiveService.takeSeat(REQUEST_DEFAULT_INDEX, REQUEST_TIMEOUT, null);
        }
    }

    private void notifyConnectedUsersUpdated(List<TUIRoomDefine.SeatInfo> seatList,
                                             List<TUIRoomDefine.SeatInfo> seatedList,
                                             List<TUIRoomDefine.SeatInfo> leftList) {
        if (mCoGuestObserver != null) {
            mCoGuestObserver.onConnectedUsersUpdated(seatList, seatedList, leftList);
        }
    }

    private void notifyUserConnectionRequest(TUIRoomDefine.Request request) {
        if (mCoGuestObserver != null && (request.requestAction == TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT || request.requestAction == TUIRoomDefine.RequestAction.REQUEST_REMOTE_USER_ON_SEAT)) {
            mCoGuestObserver.onUserConnectionRequest(request);
        }
    }

    private void notifyUserConnectionCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
        if (mCoGuestObserver != null && (request.requestAction == TUIRoomDefine.RequestAction.REQUEST_TO_TAKE_SEAT || request.requestAction == TUIRoomDefine.RequestAction.REQUEST_REMOTE_USER_ON_SEAT)) {
            mCoGuestObserver.onUserConnectionCancelled(request, operateUser);
        }
    }

    private void notifyConnectionTerminated(String userId) {
        if (mCoGuestObserver != null) {
            mCoGuestObserver.onUserConnectionTerminated(userId);
        }
    }

    private void notifyUserConnectionExited(List<TUIRoomDefine.SeatInfo> leftList) {
        if (mCoGuestObserver != null && !leftList.isEmpty()) {
            for (TUIRoomDefine.SeatInfo seatInfo : leftList) {
                if (!isSelfSeatInfo(seatInfo)) {
                    mCoGuestObserver.onUserConnectionExited(seatInfo);
                }
            }
        }
    }

    public interface CoGuestObserver {
        void onConnectedUsersUpdated(List<TUIRoomDefine.SeatInfo> userList, List<TUIRoomDefine.SeatInfo> joinList,
                                     List<TUIRoomDefine.SeatInfo> leftList);

        void onUserConnectionRequest(TUIRoomDefine.Request request);

        void onUserConnectionCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser);

        void onUserConnectionTerminated(String userId);

        void onUserConnectionExited(TUIRoomDefine.SeatInfo liveUser);
    }
}
