package com.trtc.uikit.livekit.voiceroom.manager.module;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.voiceroom.manager.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class UserManager extends BaseManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getVoiceRoomLogger("UserManager");

    public UserManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
        initSelfUserData();
    }

    @Override
    public void destroy() {
        LOGGER.info("destroy");
    }

    public void getAudienceList() {
        mLiveService.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mUserState.userList.getValue().clear();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
                            continue;
                        }
                        mUserState.userList.getValue().put(userInfo.userId, userInfo);
                    }
                    mUserState.userList.setValue(mUserState.userList.getValue());
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getUserList,error:" + error + ",message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void muteAllRemoteAudio(boolean isMute) {
        mLiveService.muteAllRemoteAudio(isMute);
    }

    public void updateOwnerUserInfo() {
        String ownerId = mRoomState.ownerInfo.userId;
        if (TextUtils.isEmpty(ownerId)) {
            return;
        }
        if (ownerId.equals(mUserState.selfInfo.userId)) {
            mUserState.selfInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
        }
        mLiveService.getUserInfo(ownerId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mRoomState.ownerInfo.userId = userInfo.userId;
                mRoomState.ownerInfo.userName = userInfo.userName;
                mRoomState.ownerInfo.avatarUrl = userInfo.avatarUrl;
                mRoomState.ownerInfo.userRole = userInfo.userRole;
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("getUserInfo failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void follow(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        mLiveService.followUser(userIDList, new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
            @Override
            public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                updateFollowUserList(userId, true);
            }

            @Override
            public void onError(int code, String desc) {
                LOGGER.error("followUser failed:errorCode:" + "message:" + desc);
                ToastUtil.toastShortMessage(code + "," + desc);
            }
        });
    }

    public void unfollow(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        mLiveService.unfollowUser(userIDList, new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
            @Override
            public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                updateFollowUserList(userId, false);
            }

            @Override
            public void onError(int code, String desc) {
                LOGGER.error("unfollowUser failed:errorCode:" + "message:" + desc);
                ToastUtil.toastShortMessage(code + "," + desc);
            }
        });
    }

    public void checkFollowUser(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        checkFollowUserList(userIDList);
    }

    private void checkFollowUserList(List<String> userIDList) {
        mLiveService.checkFollowType(userIDList, new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
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

    private void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mUserState.selfInfo.userId = loginUserInfo.userId;
        mUserState.selfInfo.userName = loginUserInfo.userName;
        mUserState.selfInfo.avatarUrl = loginUserInfo.avatarUrl;
    }

    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            return;
        }
        mUserState.userList.getValue().put(userInfo.userId, userInfo);
        mUserState.userList.setValue(mUserState.userList.getValue());
    }

    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        mUserState.userList.getValue().remove(userInfo.userId);
        mUserState.userList.setValue(mUserState.userList.getValue());
    }

    public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
        List<String> userIdList = new ArrayList<>();
        for (V2TIMUserFullInfo userInfo : userInfoList) {
            userIdList.add(userInfo.getUserID());
        }
        checkFollowUserList(userIdList);
    }

    private void updateFollowUserList(String userId, boolean isAdd) {
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        Set<String> followingUserList = mUserState.followingUserList.getValue();
        if (followingUserList == null) {
            followingUserList = new LinkedHashSet<>();
        }
        if (isAdd) {
            followingUserList.add(userId);
        } else {
            followingUserList.remove(userId);
        }
        mUserState.followingUserList.setValue(followingUserList);
    }
}
