package com.trtc.uikit.livekit.voiceroom.manager.module;

import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST;
import static com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult.V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMFollowOperationResult;
import com.tencent.imsdk.v2.V2TIMFollowTypeCheckResult;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.livekit.voiceroom.api.IVoiceRoom;
import com.trtc.uikit.livekit.voiceroom.api.Logger;
import com.trtc.uikit.livekit.voiceroom.manager.error.ErrorLocalized;
import com.trtc.uikit.livekit.voiceroom.state.UserState;
import com.trtc.uikit.livekit.voiceroom.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class UserManager extends BaseManager {
    private static final String FILE = "UserManager";

    public UserManager(VoiceRoomState state, IVoiceRoom service) {
        super(state, service);
        initSelfUserData();
    }

    @Override
    public void destroy() {
        Logger.info(FILE, " destroy");
    }

    public void getAudienceList() {
        mLiveService.getUserList(0, new TUIRoomDefine.GetUserListCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserListResult userListResult) {
                if (!userListResult.userInfoList.isEmpty()) {
                    mUserState.userList.get().clear();
                    Set<UserState.UserInfo> userInfoSet = new LinkedHashSet<>();
                    for (TUIRoomDefine.UserInfo userInfo : userListResult.userInfoList) {
                        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
                            continue;
                        }
                        UserState.UserInfo liveUserInfo = new UserState.UserInfo(userInfo);
                        userInfoSet.add(liveUserInfo);
                    }
                    mUserState.addUserList(userInfoSet);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE, "getAudienceList,error:" + error + ",message:" + message);
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
            mUserState.selfInfo.role.set(TUIRoomDefine.Role.ROOM_OWNER);
        }
        mLiveService.getUserInfo(ownerId, new TUIRoomDefine.GetUserInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.UserInfo userInfo) {
                mRoomState.ownerInfo.updateState(userInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
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
                mUserState.myFollowingUserList.add(new UserState.UserInfo(userId));
                getFansCount();
                ErrorLocalized.onError(TUICommonDefine.Error.SUCCESS);
            }

            @Override
            public void onError(int code, String desc) {
                ErrorLocalized.onError(TUICommonDefine.Error.FAILED);
            }
        });
    }

    public void unfollow(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        mLiveService.unfollowUser(userIDList, new V2TIMValueCallback<List<V2TIMFollowOperationResult>>() {
            @Override
            public void onSuccess(List<V2TIMFollowOperationResult> v2TIMFollowOperationResults) {
                mUserState.myFollowingUserList.remove(new UserState.UserInfo(userId));
                getFansCount();
                ErrorLocalized.onError(TUICommonDefine.Error.SUCCESS);
            }

            @Override
            public void onError(int code, String desc) {
                ErrorLocalized.onError(TUICommonDefine.Error.FAILED);
            }
        });
    }

    public void checkFollowType(String userId) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);
        mLiveService.checkFollowType(userIDList, new V2TIMValueCallback<List<V2TIMFollowTypeCheckResult>>() {
            @Override
            public void onSuccess(List<V2TIMFollowTypeCheckResult> v2TIMFollowTypeCheckResults) {
                if (v2TIMFollowTypeCheckResults != null && !v2TIMFollowTypeCheckResults.isEmpty()) {
                    V2TIMFollowTypeCheckResult result = v2TIMFollowTypeCheckResults.get(0);
                    if (result == null) {
                        return;
                    }
                    UserState.UserInfo userInfo = new UserState.UserInfo(result.getUserID());
                    if (V2TIM_FOLLOW_TYPE_IN_MY_FOLLOWING_LIST == result.getFollowType()
                            || V2TIM_FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST == result.getFollowType()) {
                        mUserState.myFollowingUserList.add(userInfo);
                    } else {
                        mUserState.myFollowingUserList.remove(userInfo);
                    }
                }
            }

            @Override
            public void onError(int code, String desc) {
            }
        });
    }

    public void getFansCount() {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(mRoomState.ownerInfo.userId);
        mLiveService.getUserFollowInfo(userIDList, new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
            @Override
            public void onSuccess(List<V2TIMFollowInfo> v2TIMFollowInfos) {
                if (v2TIMFollowInfos != null && !v2TIMFollowInfos.isEmpty()) {
                    V2TIMFollowInfo result = v2TIMFollowInfos.get(0);
                    if (result != null) {
                        mRoomState.ownerInfo.fansCount.set(result.getFollowersCount());
                    }
                }
            }

            @Override
            public void onError(int code, String desc) {
            }
        });
    }

    private void initSelfUserData() {
        TUIRoomDefine.LoginUserInfo loginUserInfo = TUIRoomEngine.getSelfInfo();
        mUserState.selfInfo.userId = loginUserInfo.userId;
        mUserState.selfInfo.name.set(loginUserInfo.userName);
        mUserState.selfInfo.avatarUrl.set(loginUserInfo.avatarUrl);
    }

    public void onRemoteUserEnterRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        if (userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            return;
        }
        mUserState.addUser(userInfo);
    }

    public void onRemoteUserLeaveRoom(String roomId, TUIRoomDefine.UserInfo userInfo) {
        mUserState.removeUser(userInfo);
    }
}
