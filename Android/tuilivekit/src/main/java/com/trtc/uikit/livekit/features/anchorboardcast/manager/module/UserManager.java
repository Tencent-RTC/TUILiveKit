package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorAPI;
import com.trtc.uikit.livekit.features.anchorboardcast.state.AnchorState;
import com.trtc.uikit.livekit.features.anchorboardcast.state.UserState;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class UserManager extends BaseManager {
    private static final LiveKitLogger LOGGER                     = LiveKitLogger.getFeaturesLogger("UserManager");
    private static final int           VOLUME_CAN_HEARD_MIN_LIMIT = 25;

    public UserManager(AnchorState state, IAnchorAPI service) {
        super(state, service);
    }

    @Override
    public void destroy() {
    }

    public void getAudienceList() {
        mLiveService.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mUserState.userList.getValue().clear();
                    Set<UserState.UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mCoreState.roomState.ownerInfo.getValue().userId)) {
                            continue;
                        }
                        UserState.UserInfo liveUserInfo = new UserState.UserInfo(userInfo);
                        userInfoSet.add(liveUserInfo);
                    }
                    addUserList(userInfoSet);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
            }
        });
    }

    public UserState.UserInfo getUserFromUserList(String userId) {
        if (TextUtils.isEmpty(userId)) {
            return null;
        }
        for (UserState.UserInfo userInfo : mUserState.userList.getValue()) {
            if (userId.equals(userInfo.userId)) {
                return userInfo;
            }
        }
        return null;
    }

    public UserState.UserInfo addUserInUserList(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return null;
        }

        if (TextUtils.isEmpty(userInfo.userId)) {
            return null;
        }
        UserState.UserInfo user = new UserState.UserInfo(userInfo);
        addUser(user);
        mLiveService.getUserInfo(user.userId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                user.updateState(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
            }
        });
        return user;
    }

    public void disableSendingMessageByAdmin(String userId, boolean isDisable) {
        mLiveService.disableSendingMessageByAdmin(userId, isDisable, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                UserState.UserInfo userInfo = getUserFromUserList(userId);
                if (userInfo != null) {
                    userInfo.isMessageDisabled.setValue(isDisable);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("disableSendingMessageByAdmin failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void kickRemoteUserOutOfRoom(String userId) {
        mLiveService.kickRemoteUserOutOfRoom(userId, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                TUIRoomDefine.UserInfo userInfo = new TUIRoomDefine.UserInfo();
                userInfo.userId = userId;
                removeUser(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("disableSendingMessageByAdmin failed:error:" + error + ",errorCode:" + error.getValue() +
                        "message:" + message);
                ErrorLocalized.onError(error);
            }
        });
    }

    public void getUserInfo(String userId, TUIRoomDefine.GetUserInfoCallback callback) {
        mLiveService.getUserInfo(userId, callback);
    }

    public void followUser(String userId) {
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

    public void unfollowUser(String userId) {
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
        if (userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
            return;
        }
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

    public void onUserVoiceVolumeChanged(Map<String, Integer> volumeMap) {
        for (Map.Entry<String, Integer> entry : volumeMap.entrySet()) {
            String userId = entry.getKey();
            if (entry.getValue() > VOLUME_CAN_HEARD_MIN_LIMIT) {
                mUserState.speakingUserList.getValue().add(userId);
            } else {
                mUserState.speakingUserList.getValue().remove(userId);
            }
        }
        mUserState.speakingUserList.setValue(mUserState.speakingUserList.getValue());
    }

    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mCoreState.roomState.ownerInfo.getValue().userId)) {
            return;
        }
        UserState.UserInfo user = new UserState.UserInfo(userInfo);
        addUser(user);
    }

    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        removeUser(userInfo);
    }

    public void onUserInfoChanged(TUIRoomDefine.UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
        Set<UserState.UserInfo> userList = mUserState.userList.getValue();
        for (UserState.UserInfo info : userList) {
            if (TextUtils.equals(info.userId, userInfo.userId)) {
                if (modifyFlag.contains(TUIRoomDefine.UserInfoModifyFlag.USER_ROLE)) {
                    info.role.setValue(userInfo.userRole);
                }
                break;
            }
        }
    }

    public void onMyFollowingListChanged(List<V2TIMUserFullInfo> userInfoList, boolean isAdd) {
        List<String> userIdList = new ArrayList<>();
        for (V2TIMUserFullInfo userInfo : userInfoList) {
            userIdList.add(userInfo.getUserID());
        }
        checkFollowUserList(userIdList);
    }

    public void onSendMessageForUserDisableChanged(String roomId, String userId, boolean isDisable) {
        UserState.UserInfo userInfo = getUserFromUserList(userId);
        if (userInfo != null) {
            userInfo.isMessageDisabled.setValue(isDisable);
        }

        if (!userId.equals(mCoreState.userState.selfInfo.getValue().userId)) {
            return;
        }
        if (isDisable) {
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(R.string.common_send_message_disabled));
        } else {
            ToastUtil.toastShortMessage(ContextProvider.getApplicationContext().getResources()
                    .getString(R.string.common_send_message_enable));
        }
    }

    public void addUserList(Set<UserState.UserInfo> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        mUserState.userList.getValue().addAll(list);
        mUserState.userList.setValue(mUserState.userList.getValue());
    }

    public void addUser(UserState.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        mUserState.userList.getValue().add(userInfo);
        mUserState.userList.setValue(mUserState.userList.getValue());
    }

    public void removeUser(TUIRoomDefine.UserInfo userInfo) {
        if (userInfo == null) {
            return;
        }
        if (TextUtils.isEmpty(userInfo.userId)) {
            return;
        }
        mUserState.userList.getValue().remove(new UserState.UserInfo(userInfo.userId));
        mUserState.userList.setValue(mUserState.userList.getValue());
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
