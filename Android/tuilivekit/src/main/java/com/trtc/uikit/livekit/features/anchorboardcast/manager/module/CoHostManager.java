package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;


import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.CoHostState;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class CoHostManager extends BaseManager {
    private static final LiveKitLogger LOGGER           = LiveKitLogger.getFeaturesLogger("CoHostManager");
    private static final int           FETCH_LIST_COUNT = 20;

    public CoHostManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    public List<CoHostState.ConnectionUser> getRecommendedList() {
        return mCoHostState.recommendUsers.getValue();
    }

    public boolean isSelfInCoHost() {
        List<ConnectionUser> userList = mCoreState.coHostState.connectedUserList.getValue();
        for (ConnectionUser user : userList) {
            if (TextUtils.equals(mCoreState.userState.selfInfo.getValue().userId, user.userId)) {
                return true;
            }
        }
        return false;
    }

    public void fetchLiveList(boolean isRefresh) {
        if (isRefresh) {
            mCoHostState.recommendedCursor = "";
        }
        mCoHostState.isLoadMore = true;
        mLiveService.fetchLiveList(mCoHostState.recommendedCursor, FETCH_LIST_COUNT,
                new TUILiveListManager.LiveInfoListCallback() {
                    @Override
                    public void onSuccess(TUILiveListManager.LiveInfoListResult result) {
                        handlerFetchLiveListSuccess(result, isRefresh);
                        mCoHostState.isLoadMore = false;
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error("fetchLiveList failed:error:" + error + ",errorCode:" + error.getValue() +
                                "message:" + message);
                        ErrorLocalized.onError(error);
                        mCoHostState.isLoadMore = false;
                    }
                });
    }

    public boolean isConnected(String roomId) {
        for (ConnectionUser connectionUser : mCoreState.coHostState.connectedUserList.getValue()) {
            if (TextUtils.equals(connectionUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    public void setCoHostTemplateId(int id) {
        mCoHostState.coHostTemplateId = id;
    }

    /******************************************  Observer *******************************************/
    public void onConnectionUserListChanged(List<ConnectionUser> connectedList) {
        updateRecommendedList(connectedList);
    }

    public void onConnectionRequestReceived(ConnectionUser inviter) {
    }

    public void onConnectionRequestAccept(ConnectionUser invitee) {

    }

    public void onConnectionRequestReject(ConnectionUser invitee) {
        updateRecommendListStatus();
        ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                .getString(R.string.common_connect_request_rejected));
    }

    public void onConnectionRequestTimeout(ConnectionUser inviter, ConnectionUser invitee) {
        if (TextUtils.equals(inviter.roomId, mRoomState.roomId)) {
            updateRecommendListStatus();
        }
    }

    @Override
    public void destroy() {
    }

    private void updateRecommendedList(List<ConnectionUser> connectedList) {
        List<CoHostState.ConnectionUser> recommendList = mCoHostState.recommendUsers.getValue();
        Iterator<CoHostState.ConnectionUser> iterator = recommendList.iterator();
        while (iterator.hasNext()) {
            CoHostState.ConnectionUser recommendInfo = iterator.next();
            for (ConnectionUser joinedInfo : connectedList) {
                if (TextUtils.equals(recommendInfo.roomId, joinedInfo.roomId)) {
                    iterator.remove();
                }
            }
        }
        mCoHostState.recommendUsers.setValue(mCoHostState.recommendUsers.getValue());
    }

    private void addLiveToRecommendList(TUILiveListManager.LiveInfoListResult result,
                                        List<CoHostState.ConnectionUser> list) {

        for (CoHostState.ConnectionUser user : mCoHostState.recommendUsers.getValue()) {
            boolean isInviting = false;
            for (ConnectionUser requestUser : mCoreState.coHostState.sentConnectionRequestList.getValue()) {
                if (user.roomId.equals(requestUser.roomId)) {
                    user.connectionStatus = CoHostState.ConnectionStatus.INVITING;
                    isInviting = true;
                    break;
                }
            }
            if (!isInviting) {
                user.connectionStatus = CoHostState.ConnectionStatus.UNKNOWN;
            }
        }

        for (TUILiveListManager.LiveInfo liveInfo : result.liveInfoList) {
            CoHostState.ConnectionUser user = new CoHostState.ConnectionUser(liveInfo);
            if (!isConnected(liveInfo.roomId)) {
                if (isInviting(liveInfo.roomId)) {
                    user.connectionStatus = CoHostState.ConnectionStatus.INVITING;
                }
                list.add(user);
            }
        }
    }

    private boolean isInviting(String roomId) {
        for (ConnectionUser inviteUser : mCoreState.coHostState.sentConnectionRequestList.getValue()) {
            if (TextUtils.equals(inviteUser.roomId, roomId)) {
                return true;
            }
        }
        return false;
    }

    private void updateRecommendListStatus() {
        for (CoHostState.ConnectionUser user : mCoHostState.recommendUsers.getValue()) {
            boolean isInviting = false;
            for (ConnectionUser requestUser : mCoreState.coHostState.sentConnectionRequestList.getValue()) {
                if (user.roomId.equals(requestUser.roomId)) {
                    user.connectionStatus = CoHostState.ConnectionStatus.INVITING;
                    isInviting = true;
                    break;
                }
            }
            if (!isInviting) {
                user.connectionStatus = CoHostState.ConnectionStatus.UNKNOWN;
            }
        }
        mCoHostState.recommendUsers.setValue(mCoHostState.recommendUsers.getValue());
    }

    private void handlerFetchLiveListSuccess(TUILiveListManager.LiveInfoListResult result, boolean isRefresh) {
        mCoHostState.isLastPage = TextUtils.isEmpty(result.cursor);
        List<CoHostState.ConnectionUser> list = isRefresh ? new ArrayList<>() : mCoHostState.recommendUsers.getValue();
        addLiveToRecommendList(result, list);
        mCoHostState.recommendedCursor = result.cursor;
        mCoHostState.recommendUsers.setValue(list);
    }

    public void setCoHostLayoutTemplateId(int templateId) {
        mLiveService.setCoHostLayoutTemplateId(templateId);
    }
}
