package com.trtc.uikit.component.roominfo.service;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.component.roominfo.store.RoomInfoState;

import java.util.ArrayList;
import java.util.List;

public class RoomInfoService {
    public RoomInfoState mRoomInfoState = new RoomInfoState();

    public void initRoomInfo(String roomId) {
        mRoomInfoState.roomId = roomId;
        mRoomInfoState.myUserId = TUILogin.getUserId();
        TUIRoomEngine.sharedInstance().fetchRoomInfo(roomId, TUIRoomDefine.RoomType.LIVE,
                new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        mRoomInfoState.ownerId.set(roomInfo.ownerId);
                        mRoomInfoState.ownerName.set(roomInfo.ownerName);
                        mRoomInfoState.ownerAvatarUrl.set(roomInfo.ownerAvatarUrl);
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                    }
                });
    }

    public void getFansNumber() {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(mRoomInfoState.ownerId.get());

        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> v2TIMFollowInfos) {
                        if (v2TIMFollowInfos != null && !v2TIMFollowInfos.isEmpty()) {
                            V2TIMFollowInfo result = v2TIMFollowInfos.get(0);
                            if (result != null) {
                                mRoomInfoState.fansNumber.set(result.getFollowersCount());
                            }
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {

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
                                mRoomInfoState.followingList.add(result.getUserID());
                            } else {
                                mRoomInfoState.followingList.remove(result.getUserID());
                            }
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {

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
                        mRoomInfoState.followingList.add(userId);
                        getFansNumber();
                    }

                    @Override
                    public void onError(int code, String desc) {
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
                        mRoomInfoState.followingList.remove(userId);
                        getFansNumber();
                    }

                    @Override
                    public void onError(int code, String desc) {
                    }
                });
    }
}
