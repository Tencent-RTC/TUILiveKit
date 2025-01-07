package com.trtc.uikit.livekit.livestreamcore.manager.module;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.livestreamcore.R;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class CoHostManager extends BaseManager {

    private final CoHostState    mCoHostState;
    private       CoHostObserver mCoHostObserver;

    public CoHostManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
        mCoHostState = state.coHostState;
    }

    public void setCoHostObserver(CoHostObserver observer) {
        mCoHostObserver = observer;
    }

    public boolean isEnable() {
        return mVideoLiveState.coHostState.enableConnection;
    }

    public void setEnableConnection(boolean enable) {
        mVideoLiveState.coHostState.enableConnection = enable;
    }

    public boolean isMixStreamUserId(String userId) {
        String mixStreamIdSuffix =  "_feedback_" + mVideoLiveState.roomState.roomId;
        return userId != null && userId.endsWith(mixStreamIdSuffix);
    }

    public boolean hasMixStreamUser() {
        List<ConnectionUser> connectedUserList = mCoHostState.connectedUserList.get();
        for (int i = 0; i < connectedUserList.size(); i++) {
            if (isMixStreamUserId(connectedUserList.get(i).userId)) {
                return true;
            }
        }
        return false;
    }

    public void requestConnection(String roomId, int timeoutSeconds, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.requestConnection(Collections.singletonList(roomId), timeoutSeconds, "",
                new TUILiveConnectionManager.ConnectionRequestCallback() {
                    @Override
                    public void onSuccess(Map<String, TUILiveConnectionManager.ConnectionCode> map) {
                        if (map != null) {
                            TUILiveConnectionManager.ConnectionCode code = map.get(roomId);
                            if (code == TUILiveConnectionManager.ConnectionCode.SUCCESS) {
                                ConnectionUser connectionUser = new ConnectionUser();
                                connectionUser.roomId = roomId;
                                addSendConnectionRequest(connectionUser);
                                if (callback != null) {
                                    callback.onSuccess();
                                }
                            } else {
                                ConnectionErrorHandler.onError(code);
                            }
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String s) {
                        if (callback != null) {
                            callback.onError(TUICommonDefine.Error.FAILED, s);
                        }
                    }
                });
    }

    public void cancelRequest(String roomId, TUIRoomDefine.ActionCallback callback) {
        removeSendConnectionRequest(roomId);

        mVideoLiveService.cancelConnectionRequest(Collections.singletonList(roomId), callback);
    }

    public void accept(String roomId, TUIRoomDefine.ActionCallback callback) {
        removeReceivedConnectionRequest();
        mVideoLiveService.acceptConnection(roomId, callback);
    }

    public void reject(String roomId, TUIRoomDefine.ActionCallback callback) {
        removeReceivedConnectionRequest();
        mVideoLiveService.rejectConnection(roomId, callback);
    }

    public void disconnect() {
        mVideoLiveService.disconnect(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveState.coHostState.connectedUserList.clear();
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
    }

    public void addSendConnectionRequest(ConnectionUser user) {
        mVideoLiveState.coHostState.sentConnectionRequestList.add(user);
    }

    /******************************************  Observer *******************************************/
    public void onConnectionUserListChanged(List<ConnectionUser> connectedList,
                                            List<ConnectionUser> joinedList,
                                            List<ConnectionUser> leavedList) {
        updateConnectedUsers(connectedList);

        notifyConnectedRoomsUpdated();
        notifyCrossRoomConnectionExited(leavedList);
    }

    public void onConnectionRequestReceived(ConnectionUser inviter,
                                            List<ConnectionUser> inviteeList,
                                            String extensionInfo) {
        if (mVideoLiveState.coGuestState.connectedUserList.get().size() > 1
                || !mVideoLiveState.coGuestState.connectionRequestList.get().isEmpty()
                || !mVideoLiveState.coGuestState.myRequestId.get().isEmpty()) {
            reject(inviter.roomId, null);
            return;
        }
        addReceivedConnectionRequest(inviter);

        notifyCrossRoomConnectionRequest(inviter);
    }

    public void onConnectionRequestCanceled(ConnectionUser inviter) {
        removeReceivedConnectionRequest();

        notifyCrossRoomConnectionCancelled(inviter);
    }


    public void onConnectionRequestAccept(ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);

        notifyCrossRoomConnectionAccepted(invitee);
    }

    public void onConnectionRequestReject(ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);

        notifyCrossRoomConnectionRejected(invitee);
    }

    public void onConnectionRequestTimeout(ConnectionUser inviter,
                                           ConnectionUser invitee) {
        if (TextUtils.equals(inviter.roomId, mVideoLiveState.roomState.roomId)) {
            removeSendConnectionRequest(invitee.roomId);
        } else {
            removeReceivedConnectionRequest();
        }

        notifyCrossRoomConnectionTimeout(inviter, invitee);
    }

    @Override
    public void destroy() {

    }

    private void updateConnectedUsers(List<ConnectionUser> connectedList) {
        mVideoLiveState.coHostState.connectedUserList.get().clear();
        mVideoLiveState.coHostState.connectedUserList.addAll(connectedList);
    }

    private void removeSendConnectionRequest(String inviteeRoomId) {
        Iterator<ConnectionUser> iterator =
                mVideoLiveState.coHostState.sentConnectionRequestList.get().listIterator();
        while (iterator.hasNext()) {
            ConnectionUser user = iterator.next();
            if (TextUtils.equals(user.roomId, inviteeRoomId)) {
                iterator.remove();
            }
        }
        mVideoLiveState.coHostState.sentConnectionRequestList.notifyDataChanged();
    }

    private void addReceivedConnectionRequest(ConnectionUser inviter) {
        mVideoLiveState.coHostState.receivedConnectionRequest.set(inviter);
    }

    private void removeReceivedConnectionRequest() {
        mVideoLiveState.coHostState.receivedConnectionRequest.set(null);
    }

    private void notifyConnectedRoomsUpdated() {
        if (mCoHostObserver != null) {
            mCoHostObserver.onConnectedRoomsUpdated(mCoHostState.connectedUserList.get());
        }
    }

    private void notifyCrossRoomConnectionRequest(ConnectionUser userInfo) {
        if (mCoHostObserver != null) {
            mCoHostObserver.onCrossRoomConnectionRequest(userInfo);
        }
    }

    private void notifyCrossRoomConnectionCancelled(ConnectionUser userInfo) {
        if (mCoHostObserver != null) {
            mCoHostObserver.onCrossRoomConnectionCancelled(userInfo);
        }
    }

    private void notifyCrossRoomConnectionAccepted(ConnectionUser invitee) {
        if (mCoHostObserver != null) {
            mCoHostObserver.onCrossRoomConnectionAccepted(invitee);
        }
    }

    private void notifyCrossRoomConnectionRejected(ConnectionUser invitee) {
        if (mCoHostObserver != null) {
            mCoHostObserver.onCrossRoomConnectionRejected(invitee);
        }
    }

    private void notifyCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {
        if (mCoHostObserver != null) {
            mCoHostObserver.onCrossRoomConnectionTimeout(inviter, invitee);
        }
    }

    private void notifyCrossRoomConnectionExited(List<ConnectionUser> leftList) {
        if (mCoHostObserver != null && !leftList.isEmpty()) {
            for (TUILiveConnectionManager.ConnectionUser user : leftList) {
                mCoHostObserver.onCrossRoomConnectionExited(user);
            }
        }
    }

    public interface CoHostObserver {
        void onConnectedRoomsUpdated(List<ConnectionUser> userList);

        void onCrossRoomConnectionRequest(TUILiveConnectionManager.ConnectionUser user);

        void onCrossRoomConnectionCancelled(TUILiveConnectionManager.ConnectionUser user);

        void onCrossRoomConnectionAccepted(TUILiveConnectionManager.ConnectionUser user);

        void onCrossRoomConnectionRejected(TUILiveConnectionManager.ConnectionUser user);

        void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee);

        void onCrossRoomConnectionExited(ConnectionUser user);
    }

    private static class ConnectionErrorHandler {

        public static void onError(TUILiveConnectionManager.ConnectionCode code) {
            if (code == TUILiveConnectionManager.ConnectionCode.SUCCESS || code == null) {
                return;
            }
            String message = convertToErrorMessage(code);
            Logger.info("ConnectionErrorHandler :[code:" + code + ",message:" + message + "]");
            showToast(message);
        }

        private static String convertToErrorMessage(TUILiveConnectionManager.ConnectionCode resultCode) {
            Context context = ContextProvider.getApplicationContext();
            switch (resultCode) {
                case CONNECTING:
                case CONNECTING_OTHER_ROOM:
                    return context.getString(R.string.livestreamcore_cohost_conflict);
                case CONNECTION_FULL:
                    return context.getString(R.string.livestreamcore_cohost_room_full);
                default:
                    return context.getString(R.string.livestreamcore_cohost_error);
            }
        }

        private static void showToast(String tips) {
            Context context = ContextProvider.getApplicationContext();
            View view = LayoutInflater.from(context).inflate(R.layout.livestreamcore_connection_toast, null, false);

            TextView text = view.findViewById(R.id.tv_toast_text);
            text.setText(tips);
            ImageView image = view.findViewById(R.id.iv_toast_image);
            image.setImageResource(R.drawable.livestreamcore_connection_toast_icon);

            Toast toast = new Toast(view.getContext());
            toast.setDuration(Toast.LENGTH_SHORT);
            toast.setView(view);
            toast.show();
        }
    }
}
