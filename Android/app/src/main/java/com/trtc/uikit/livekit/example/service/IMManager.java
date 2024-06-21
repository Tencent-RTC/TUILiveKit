package com.trtc.uikit.livekit.example.service;

import android.text.TextUtils;
import android.util.Log;

import com.tencent.imsdk.v2.V2TIMCallback;
import com.tencent.imsdk.v2.V2TIMFollowInfo;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.trtc.uikit.livekit.example.store.AppStore;

import java.util.ArrayList;
import java.util.List;

public class IMManager {
    private static final String TAG = "IMManager";

    public static void getUserFollowInfo(String userId, ICallBack callBack) {
        List<String> userIDList = new ArrayList<>();
        userIDList.add(userId);

        V2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList,
                new V2TIMValueCallback<List<V2TIMFollowInfo>>() {
                    @Override
                    public void onSuccess(List<V2TIMFollowInfo> v2TIMFollowInfos) {
                        if (v2TIMFollowInfos != null && !v2TIMFollowInfos.isEmpty()) {
                            V2TIMFollowInfo result = v2TIMFollowInfos.get(0);
                            if (result != null) {
                                AppStore.followCount = result.getFollowingCount();
                                AppStore.fansCount = result.getFollowersCount();
                            }
                        }
                        if (callBack != null) {
                            callBack.onSuccess();
                        }
                    }

                    @Override
                    public void onError(int code, String message) {
                        Log.e(TAG, "getUserFollowInfo onError:[code=" + code + ", message=" + message + "]");
                        if (callBack != null) {
                            callBack.onError(code, message);
                        }
                    }
                });
    }

    public static void setSelfInfo(String avatar, String nickname, ICallBack callBack) {
        if (TextUtils.isEmpty(nickname)) {
            return;
        }
        V2TIMUserFullInfo v2TIMUserFullInfo = new V2TIMUserFullInfo();
        v2TIMUserFullInfo.setNickname(nickname);
        v2TIMUserFullInfo.setFaceUrl(avatar);
        V2TIMManager.getInstance().setSelfInfo(v2TIMUserFullInfo, new V2TIMCallback() {
            @Override
            public void onSuccess() {
                AppStore.userName = nickname;
                AppStore.userAvatar = avatar;
                if (callBack != null) {
                    callBack.onSuccess();
                }
            }

            @Override
            public void onError(int code, String message) {
                if (callBack != null) {
                    callBack.onError(code, message);
                }
            }
        });
    }
}
