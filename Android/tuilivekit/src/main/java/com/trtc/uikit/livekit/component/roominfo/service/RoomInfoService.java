package com.trtc.uikit.livekit.component.roominfo.service;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMFriendshipListener;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.roominfo.store.RoomInfoState;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class RoomInfoService {
    private static final LiveKitLogger LOGGER         = LiveKitLogger.getComponentLogger("RoomInfoService");
    public               RoomInfoState mRoomInfoState = new RoomInfoState();

    public void init(TUILiveListManager.LiveInfo liveInfo) {
        mRoomInfoState.roomId = liveInfo.roomId;
        mRoomInfoState.myUserId = TUILogin.getUserId();
        mRoomInfoState.ownerId.setValue(liveInfo.ownerId);
        mRoomInfoState.ownerName.setValue(liveInfo.ownerName);
        mRoomInfoState.ownerAvatarUrl.setValue(liveInfo.ownerAvatarUrl);
        V2TIMManager.getFriendshipManager().addFriendListener(mTIMFriendshipListener);
    }

    public void unInit() {
        V2TIMManager.getFriendshipManager().removeFriendListener(mTIMFriendshipListener);
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

    public void checkFollowUserList(List<String> userIDList) {
        V2TIMManager.getFriendshipManager().checkFollowType(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowTypeCheckResult> v2TIMFollowTypeCheckResults) {
                        if (v2TIMFollowTypeCheckResults != null && !v2TIMFollowTypeCheckResults.isEmpty()) {
                            V2TIMFollowTypeCheckResult result = v2TIMFollowTypeCheckResults.get(0);
                            if (result == null) {
                                return;
                            }
                            boolean isAdd = V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST == result.getFollowType()
                                    || V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST == result.getFollowType();
                            updateFollowUserList(result.getUserID(), isAdd);
                        }
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("checkFollowType failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }

    public void checkFollowUser(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        checkFollowUserList(userIDList);
    }

    public void followUser(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        V2TIMManager.getFriendshipManager().followUser(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                        updateFollowUserList(userId, true);
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
                        updateFollowUserList(userId, false);
                        getFansNumber();
                    }

                    @Override
                    public void onError(int code, String desc) {
                        LOGGER.error("unfollowUser failed:errorCode:" + "message:" + desc);
                        ToastUtil.toastShortMessage(code + "," + desc);
                    }
                });
    }

    private void updateFollowUserList(String userId, boolean isAdd) {
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        Set<String> followingUserList = mRoomInfoState.followingList.getValue();
        if (followingUserList == null) {
            followingUserList = new LinkedHashSet<>();
        }
        if (isAdd) {
            followingUserList.add(userId);
        } else {
            followingUserList.remove(userId);
        }
        mRoomInfoState.followingList.setValue(followingUserList);
    }

    private final V2TIMFriendshipListener mTIMFriendshipListener = new V2TIMFriendshipListener() {
        @Override
        public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
            List<String> userIdList = new ArrayList<>();
            for (V2TIMUserFullInfo userInfo : userInfoList) {
                userIdList.add(userInfo.getUserID());
            }
            checkFollowUserList(userIdList);
        }
    };
}
