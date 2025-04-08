package com.trtc.uikit.livekit.component.roominfo.service;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.roominfo.store.RoomInfoState;

import java.util.ArrayList;
import java.util.List;

public class RoomInfoService {
    private static final LiveKitLogger LOGGER         = LiveKitLogger.getComponentLogger("RoomInfoService");
    public               RoomInfoState mRoomInfoState = new RoomInfoState();

    public RoomInfoService() {
        V2TIMManager.getFriendshipManager().addFriendListener(new V2TIMFriendshipListener() {
            @Override
            public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
                for (V2TIMUserFullInfo userInfo : userInfoList) {
                    if (isAdd) {
                        mRoomInfoState.followingList.getValue().add(userInfo.getUserID());
                    } else {
                        mRoomInfoState.followingList.getValue().remove(userInfo.getUserID());
                    }
                }
                mRoomInfoState.followingList.setValue(mRoomInfoState.followingList.getValue());
            }
        });
    }

    public void initRoomInfo(String roomId) {
        mRoomInfoState.roomId = roomId;
        mRoomInfoState.myUserId = TUILogin.getUserId();
        TUIRoomEngine.sharedInstance().fetchRoomInfo(roomId, TUIRoomDefine.RoomType.LIVE,
                new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        mRoomInfoState.ownerId.setValue(roomInfo.ownerId);
                        mRoomInfoState.ownerName.setValue(roomInfo.ownerName);
                        mRoomInfoState.ownerAvatarUrl.setValue(roomInfo.ownerAvatarUrl);
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        LOGGER.error("fetchRoomInfo failed:error:" + error + ",errorCode:" + error.getValue() +
                                "message:" + message);
                        ErrorLocalized.onError(error);
                    }
                });
    }

    public void getFansNumber() {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(mRoomInfoState.ownerId.getValue());

        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> v2TIMFollowInfos) {
                        if (v2TIMFollowInfos != null && !v2TIMFollowInfos.isEmpty()) {
                            V2TIMFollowInfo result = v2TIMFollowInfos.get(0);
                            if (result != null) {
                                mRoomInfoState.fansNumber.setValue(result.getFollowersCount());
                            }
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("getUserFollowInfo failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }

    public void isFollow(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> v2TIMFollowTypeCheckResults) {
                        if (v2TIMFollowTypeCheckResults != null && !v2TIMFollowTypeCheckResults.isEmpty()) {
                            V2TIMFollowTypeCheckResult result = v2TIMFollowTypeCheckResults.get(0);
                            if (result == null) {
                                return;
                            }
                            if (V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST == result.getFollowType()
                                    || V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST == result.getFollowType()) {
                                mRoomInfoState.followingList.getValue().add(result.getUserID());
                            } else {
                                mRoomInfoState.followingList.getValue().remove(result.getUserID());
                            }
                            mRoomInfoState.followingList.setValue(mRoomInfoState.followingList.getValue());
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("checkFollowType failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }

                });
    }

    public void followUser(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                        getFansNumber();
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("followUser failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }

    public void unfollowUser(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        V2TIMManager.getFriendshipManager().unfollowUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                        getFansNumber();
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("unfollowUser failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }
}
