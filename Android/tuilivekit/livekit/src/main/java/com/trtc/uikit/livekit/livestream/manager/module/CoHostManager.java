package com.trtc.uikit.livekit.livestream.manager.module;


import static com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionStatus.INVITING;
import static com.trtc.uikit.livekit.livestream.state.CoHostState.ConnectionStatus.UNKNOWN;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.livestream.manager.api.ILiveService;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;
import com.trtc.uikit.livekit.livestream.state.CoHostState;
import com.trtc.uikit.livekit.livestream.state.LiveState;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class CoHostManager extends BaseManager {
    private static final String TAG              = "CoHostManager";
    private static final int    FETCH_LIST_COUNT = 20;

    public CoHostManager(LiveState state, ILiveService service) {
        super(state, service);
    }

    public List<CoHostState.ConnectionUser> getRecommendedList() {
        return mCoHostState.recommendUsers.getValue();
    }

    public void addSendConnectionRequest(CoHostState.ConnectionUser user) {
        mCoHostState.sentConnectionRequests.getValue().add(user);
        mCoHostState.sentConnectionRequests.setValue(mCoHostState.sentConnectionRequests.getValue());
    }

    public void cleanConnectedUsers() {
        mCoHostState.connectedUsers.getValue().clear();
        mCoHostState.connectedUsers.setValue(mCoHostState.connectedUsers.getValue());
    }

    public void fetchLiveList() {
        mLiveService.fetchLiveList(mCoHostState.recommendedCursor, FETCH_LIST_COUNT,
                new TUILiveListManager.LiveInfoListCallback() {
                    @Override
                    public void onSuccess(TUILiveListManager.LiveInfoListResult result) {
                        handlerFetchLiveListSuccess(result);
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LiveStreamLog.error(TAG + " fetchLiveList failed:error:" + error + ",errorCode:" + error.getValue() + "message:" + message);
                        ErrorLocalized.onError(error);
                    }
                });
    }

    public boolean isConnected(String roomId) {
        for (CoHostState.ConnectionUser connectionUser : mCoHostState.connectedUsers.getValue()) {
            if (TextUtils.equals(connectionUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    /******************************************  Observer *******************************************/
    public void onConnectionUserListChanged(List<TUILiveConnectionManager.ConnectionUser> connectedList) {
        updateRecommendedList(connectedList);
        updateConnectedUsers(
                convertWithStatus(connectedList, CoHostState.ConnectionStatus.CONNECTED));
    }

    public void onConnectionRequestReceived(TUILiveConnectionManager.ConnectionUser inviter) {
        addReceivedConnectionRequest(new CoHostState.ConnectionUser(inviter, UNKNOWN));
    }

    public void onConnectionRequestAccept(TUILiveConnectionManager.ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);
    }

    public void onConnectionRequestReject(TUILiveConnectionManager.ConnectionUser invitee) {
        removeSendConnectionRequest(invitee.roomId);
        updateRecommendListStatus();
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.live_connect_request_rejected));
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
    public void destroy() {
    }

    private void updateRecommendedList(List<TUILiveConnectionManager.ConnectionUser> connectedList) {
        List<CoHostState.ConnectionUser> recommendList = mCoHostState.recommendUsers.getValue();
        Iterator<CoHostState.ConnectionUser> iterator = recommendList.iterator();
        while (iterator.hasNext()) {
            CoHostState.ConnectionUser recommendInfo = iterator.next();
            for (TUILiveConnectionManager.ConnectionUser joinedInfo : connectedList) {
                if (TextUtils.equals(recommendInfo.roomId, joinedInfo.roomId)) {
                    iterator.remove();
                }
            }
        }
        mCoHostState.recommendUsers.setValue(mCoHostState.recommendUsers.getValue());
    }

    private void updateConnectedUsers(List<CoHostState.ConnectionUser> connectedList) {
        mCoHostState.connectedUsers.getValue().clear();
        mCoHostState.connectedUsers.getValue().addAll(connectedList);
        mCoHostState.connectedUsers.setValue(mCoHostState.connectedUsers.getValue());
    }

    private void removeSendConnectionRequest(String inviteeRoomId) {
        Iterator<CoHostState.ConnectionUser> iterator =
                mCoHostState.sentConnectionRequests.getValue().listIterator();
        while (iterator.hasNext()) {
            CoHostState.ConnectionUser user = iterator.next();
            if (TextUtils.equals(user.roomId, inviteeRoomId)) {
                iterator.remove();
            }
        }
        mCoHostState.sentConnectionRequests.setValue(mCoHostState.sentConnectionRequests.getValue());
    }

    private void addReceivedConnectionRequest(CoHostState.ConnectionUser inviter) {
        mCoHostState.receivedConnectionRequest.setValue(inviter);
    }

    public void removeReceivedConnectionRequest() {
        mCoHostState.receivedConnectionRequest.setValue(null);
    }

    private List<CoHostState.ConnectionUser> convertWithStatus(
            List<TUILiveConnectionManager.ConnectionUser> src,
            CoHostState.ConnectionStatus status) {

        List<CoHostState.ConnectionUser> dest = new ArrayList<>();
        for (TUILiveConnectionManager.ConnectionUser connectionUser : src) {
            CoHostState.ConnectionUser user = new CoHostState.ConnectionUser(connectionUser, status);
            dest.add(user);
        }
        return dest;
    }

    private void addLiveToRecommendList(TUILiveListManager.LiveInfoListResult result,
                                        List<CoHostState.ConnectionUser> list) {

        for (CoHostState.ConnectionUser user : mCoHostState.recommendUsers.getValue()) {
            boolean isInviting = false;
            for (CoHostState.ConnectionUser requestUser : mCoHostState.sentConnectionRequests.getValue()) {
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
            CoHostState.ConnectionUser user = new CoHostState.ConnectionUser(liveInfo);
            if (!isConnected(liveInfo.roomInfo.roomId)) {
                if (isInviting(liveInfo.roomInfo.roomId)) {
                    user.connectionStatus = INVITING;
                }
                list.add(user);
            }
        }
    }

    private boolean isInviting(String roomId) {
        for (CoHostState.ConnectionUser inviteUser : mCoHostState.sentConnectionRequests.getValue()) {
            if (TextUtils.equals(inviteUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    private void updateRecommendListStatus() {
        for (CoHostState.ConnectionUser user : mCoHostState.recommendUsers.getValue()) {
            boolean isInviting = false;
            for (CoHostState.ConnectionUser requestUser : mCoHostState.sentConnectionRequests.getValue()) {
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
        mCoHostState.recommendUsers.setValue(mCoHostState.recommendUsers.getValue());
    }

    private void handlerFetchLiveListSuccess(TUILiveListManager.LiveInfoListResult result) {
        List<CoHostState.ConnectionUser> list = mCoHostState.recommendUsers.getValue();

        if (TextUtils.isEmpty(mCoHostState.recommendedCursor)) {
            list.clear();
        }

        addLiveToRecommendList(result, list);

        mCoHostState.recommendedCursor = result.cursor;
        mCoHostState.recommendUsers.setValue(list);
    }

}
