package com.trtc.uikit.livekit.manager.controller;


import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_REQUEST_CONNECTION;
import static com.trtc.uikit.livekit.state.operation.ConnectionState.ConnectionStatus.INVITING;
import static com.trtc.uikit.livekit.state.operation.ConnectionState.ConnectionStatus.UNKNOWN;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.state.LiveState;
import com.trtc.uikit.livekit.state.operation.ConnectionState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ConnectionController extends Controller {
    private static final String TAG              = "ConnectionController";
    private static final int    FETCH_LIST_COUNT = 20;

    public ConnectionController(LiveState state, ILiveService service) {
        super(state, service);
    }

    public List<ConnectionState.ConnectionUser> getRecommendedList() {
        return mConnectionState.recommendUsers.get();
    }

    public void requestConnection(List<String> roomIdList, int timeoutSeconds, String extensionInfo) {
        mLiveService.requestConnection(roomIdList, timeoutSeconds, extensionInfo,
                new TUILiveConnectionManager.ConnectionRequestCallback() {
                    @Override
                    public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> map) {
                        LiveKitLog.info("requestConnection :[onSuccess]");

                        for (Map.Entry<String, TUILiveConnectionManager.ConnectionCode> entry : map.entrySet()) {
                            String key = entry.getKey();
                            if (entry.getValue() == TUILiveConnectionManager.ConnectionCode.SUCCESS) {
                                for (ConnectionState.ConnectionUser recommendUser : getRecommendedList()) {
                                    if (TextUtils.equals(recommendUser.roomId, key)) {
                                        recommendUser.connectionStatus = INVITING;
                                        addSendConnectionRequest(recommendUser);
                                        mConnectionState.recommendUsers.notifyDataChanged();
                                    }
                                }
                            } else {
                                Map<String, Object> params = new HashMap<>();
                                params.put(key, entry.getValue());
                                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, params);
                            }
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String s) {
                        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_REQUEST_CONNECTION, null);
                    }
                });
    }

    public void cancelRequest(List<String> list) {
        mLiveService.cancel(list, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {

            }
        });
    }

    public void accept(String roomId) {
        mLiveService.accept(roomId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("acceptConnection :[onSuccess]");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LiveKitLog.error("acceptConnection :[onError]");
            }
        });

    }

    public void reject(String roomId) {
        mLiveService.reject(roomId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info("rejectConnection :[Success]");
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LiveKitLog.error("rejectConnection :[onError]");
            }
        });
    }

    public void disconnect() {
        mLiveService.disconnect(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mConnectionState.connectedUsers.clear();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public void addSendConnectionRequest(ConnectionState.ConnectionUser user) {
        mConnectionState.sentConnectionRequests.add(user);
    }

    public void fetchLiveList() {
        mLiveService.fetchLiveList(mConnectionState.recommendedCursor, FETCH_LIST_COUNT,
                new TUILiveListManager.LiveInfoListCallback() {
                    @Override
                    public void onSuccess(TUILiveListManager.LiveInfoListResult result) {
                        handlerFetchLiveListSuccess(result);
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String s) {
                        ErrorHandler.onError(error);
                    }
                });
    }

    /******************************************  Observer *******************************************/
    public void onConnectionUserListChanged(List<TUILiveConnectionManager.ConnectionUser> connectedList,
                                            List<TUILiveConnectionManager.ConnectionUser> joinedList,
                                            List<TUILiveConnectionManager.ConnectionUser> leavedList) {
        updateRecommendedList(connectedList);
        updateConnectedUsers(
                convertWithStatus(connectedList, ConnectionState.ConnectionStatus.CONNECTED));
    }

    public void onConnectionRequestReceived(TUILiveConnectionManager.ConnectionUser inviter,
                                            List<TUILiveConnectionManager.ConnectionUser> inviteeList,
                                            String extensionInfo) {
        addReceivedConnectionRequest(new ConnectionState.ConnectionUser(inviter, UNKNOWN));
    }

    public void onConnectionRequestAccept(TUILiveConnectionManager.ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);
    }

    public void onConnectionRequestReject(TUILiveConnectionManager.ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);
        updateRecommendListStatus();
    }

    public void onConnectionRequestTimeout(TUILiveConnectionManager.ConnectionUser inviter,
                                           TUILiveConnectionManager.ConnectionUser invitee) {
        if (TextUtils.equals(inviter.roomId, mRoomState.roomId)) {
            removeSendConnectionRequest(invitee.roomId);
            updateRecommendListStatus();
        } else {
            removeReceivedConnectionRequest();
        }
    }

    @Override
    protected void destroy() {
        LiveKitLog.info(TAG + " destroy");
    }

    private void updateRecommendedList(List<TUILiveConnectionManager.ConnectionUser> connectedList) {
        List<ConnectionState.ConnectionUser> recommendList = mConnectionState.recommendUsers.get();
        Iterator<ConnectionState.ConnectionUser> iterator = recommendList.iterator();
        while (iterator.hasNext()) {
            ConnectionState.ConnectionUser recommendInfo = iterator.next();
            for (TUILiveConnectionManager.ConnectionUser joinedInfo : connectedList) {
                if (TextUtils.equals(recommendInfo.roomId, joinedInfo.roomId)) {
                    iterator.remove();
                }
            }
        }
        mConnectionState.recommendUsers.notifyDataChanged();
    }

    private void updateConnectedUsers(List<ConnectionState.ConnectionUser> connectedList) {
        mConnectionState.connectedUsers.clear();
        mConnectionState.connectedUsers.addAll(connectedList);
    }

    private void removeSendConnectionRequest(String inviteeRoomId) {
        Iterator<ConnectionState.ConnectionUser> iterator =
                mConnectionState.sentConnectionRequests.get().listIterator();
        while (iterator.hasNext()) {
            ConnectionState.ConnectionUser user = iterator.next();
            if (TextUtils.equals(user.roomId, inviteeRoomId)) {
                iterator.remove();
            }
        }
        mConnectionState.sentConnectionRequests.notifyDataChanged();
    }

    private void addReceivedConnectionRequest(ConnectionState.ConnectionUser inviter) {
        mConnectionState.receivedConnectionRequest.set(inviter);
    }

    private void removeReceivedConnectionRequest() {
        mConnectionState.receivedConnectionRequest.set(null);
    }

    private List<ConnectionState.ConnectionUser> convertWithStatus(
            List<TUILiveConnectionManager.ConnectionUser> src,
            ConnectionState.ConnectionStatus status) {

        List<ConnectionState.ConnectionUser> dest = new ArrayList<>();
        for (TUILiveConnectionManager.ConnectionUser connectionUser : src) {
            ConnectionState.ConnectionUser user = new ConnectionState.ConnectionUser(connectionUser, status);
            dest.add(user);
        }
        return dest;
    }

    private void addLiveToRecommendList(TUILiveListManager.LiveInfoListResult result,
                                        List<ConnectionState.ConnectionUser> list) {

        for (ConnectionState.ConnectionUser user : mConnectionState.recommendUsers.get()) {
            boolean isInviting = false;
            for (ConnectionState.ConnectionUser requestUser : mConnectionState.sentConnectionRequests.get()) {
                if (user.roomId.equals(requestUser.roomId)) {
                    user.connectionStatus = INVITING;
                    isInviting = true;
                    break;
                }
            }
            if (!isInviting) {
                user.connectionStatus = UNKNOWN;
            }
        }

        for (TUILiveListManager.LiveInfo liveInfo : result.liveInfoList) {
            ConnectionState.ConnectionUser user = new ConnectionState.ConnectionUser(liveInfo);
            if (!isConnected(liveInfo.roomInfo.roomId)) {
                if (isInviting(liveInfo.roomInfo.roomId)) {
                    user.connectionStatus = INVITING;
                }
                list.add(user);
            }
        }
    }

    private boolean isConnected(String roomId) {
        for (ConnectionState.ConnectionUser connectionUser : mConnectionState.connectedUsers.get()) {
            if (TextUtils.equals(connectionUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    private boolean isInviting(String roomId) {
        for (ConnectionState.ConnectionUser inviteUser : mConnectionState.sentConnectionRequests.get()) {
            if (TextUtils.equals(inviteUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    private void updateRecommendListStatus() {
        for (ConnectionState.ConnectionUser user : mConnectionState.recommendUsers.get()) {
            boolean isInviting = false;
            for (ConnectionState.ConnectionUser requestUser : mConnectionState.sentConnectionRequests.get()) {
                if (user.roomId.equals(requestUser.roomId)) {
                    user.connectionStatus = INVITING;
                    isInviting = true;
                    break;
                }
            }
            if (!isInviting) {
                user.connectionStatus = UNKNOWN;
            }
        }
        mConnectionState.recommendUsers.notifyDataChanged();
    }

    private void handlerFetchLiveListSuccess(TUILiveListManager.LiveInfoListResult result) {
        List<ConnectionState.ConnectionUser> list = mConnectionState.recommendUsers.get();

        if (TextUtils.isEmpty(mConnectionState.recommendedCursor)) {
            list.clear();
        }

        addLiveToRecommendList(result, list);

        mConnectionState.recommendedCursor = result.cursor;
        mConnectionState.recommendUsers.set(list);
    }

}
