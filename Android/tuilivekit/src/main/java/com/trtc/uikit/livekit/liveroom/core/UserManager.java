package com.trtc.uikit.livekit.liveroom.core;

import android.text.TextUtils;

import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.liveroom.core.listener.GetUserInfoCallback;
import com.trtc.uikit.livekit.liveroom.core.listener.GetUserListCallback;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class UserManager {
    private UserManager() {
    }

    private static class UserManagerHolder {
        private static final UserManager instance = new UserManager();
    }

    public static UserManager getInstance() {
        return UserManagerHolder.instance;
    }

    public void getUserInfo(String userId, GetUserInfoCallback listener) {
        LiveKitLog.info("UserManager getUserInfo:[userId:" + userId + "]");
        if (listener != null) {
            List<String> userList = new ArrayList<>();
            userList.add(userId);
            getUserList(userList, new GetUserListCallback() {
                @Override
                public void onSuccess(List<UserInfo> userList) {
                    if (userList != null && !userList.isEmpty()) {
                        listener.onSuccess(userList.get(0));
                    }
                }

                @Override
                public void onError(int errorCode, String errorMessage) {
                    listener.onError(errorCode, errorMessage);
                }
            });
        } else {
            LiveKitLog.error("UserManager getUserInfo:[Cancel]");
        }
    }

    public void getUserList(List<String> userList, GetUserListCallback listener) {
        LiveKitLog.info("UserManager getUserList:[userList:" + userList + "]");
        if (listener != null) {
            V2TIMManager.getInstance().getUsersInfo(userList, new V2TIMValueCallback<List<V2TIMUserFullInfo>>() {
                @Override
                public void onSuccess(List<V2TIMUserFullInfo> v2TIMUserFullInfoList) {
                    LiveKitLog.info("UserManager getUserList:[Success, v2TIMUserFullInfoList:"
                            + v2TIMUserFullInfoList + "]");
                    CopyOnWriteArrayList<UserInfo> userInfoList = new CopyOnWriteArrayList<>();
                    for (V2TIMUserFullInfo userFullInfo : v2TIMUserFullInfoList) {
                        if (!TextUtils.isEmpty(userFullInfo.getUserID())) {
                            UserInfo userInfo = new UserInfo();
                            userInfo.userId = userFullInfo.getUserID();
                            userInfo.name.set(userFullInfo.getNickName());
                            userInfo.avatarUrl.set(userFullInfo.getFaceUrl());
                            userInfoList.add(userInfo);
                        }
                    }
                    listener.onSuccess(userInfoList);
                }

                @Override
                public void onError(int error, String message) {
                    LiveKitLog.error("UserManager getUserList:[Error:" + error + ",message:" + message + "]");
                    listener.onError(error, message);
                }
            });
        } else {
            LiveKitLog.error("UserManager getUserList:[Cancel]");
        }
    }
}
