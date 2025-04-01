package com.trtc.uikit.livekit.example.view.login;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.WindowManager;
import android.widget.EditText;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.tencent.qcloud.tuicore.util.SPUtils;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.tencent.qcloud.tuikit.debug.GenerateTestUserSig;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.example.view.main.MainActivity;
import com.trtc.uikit.livekit.common.utils.LiveCoreLogger;

import java.util.ArrayList;
import java.util.List;

public class LoginActivity extends BaseActivity {
    private static final String TAG = "LoginActivity";

    private EditText mEditUserId;

    private final ITUINotification mNotification = (key, subKey, param) -> {
        if (TUIConstants.TUILogin.EVENT_LOGIN_STATE_CHANGED.equals(key)
                && TUIConstants.TUILogin.EVENT_SUB_KEY_USER_LOGIN_SUCCESS.equals(subKey)
                && TUILogin.isUserLogined()) {
            TUIRoomEngine.login(TUILogin.getAppContext(), TUILogin.getSdkAppId(), TUILogin.getUserId(),
                    TUILogin.getUserSig(), new TUIRoomDefine.ActionCallback() {
                        @Override
                        public void onSuccess() {
                            LiveCoreLogger.info("TUIRoomEngine login:[Success]");
                        }

                        @Override
                        public void onError(TUICommonDefine.Error error, String message) {
                            LiveCoreLogger.error("TUIRoomEngine login:[Error:" + error + ",message:" + message + "]");
                        }
                    });
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_login);
        getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);

        mEditUserId = findViewById(R.id.et_userId);
        mEditUserId.setText(SPUtils.getInstance("livekit").getString("userId"));
        findViewById(R.id.btn_login).setOnClickListener(v -> {
            String userId = mEditUserId.getText().toString().trim();
            SPUtils.getInstance("livekit").put("userId", userId);
            login(userId);
        });
    }

    private void login(String userId) {
        if (TextUtils.isEmpty(userId)) {
            ToastUtil.toastShortMessage(getString(R.string.app_user_id_is_empty));
            return;
        }
        TUICore.registerEvent(TUIConstants.TUILogin.EVENT_LOGIN_STATE_CHANGED,
                TUIConstants.TUILogin.EVENT_SUB_KEY_USER_LOGIN_SUCCESS, mNotification);
        TUILogin.login(this, GenerateTestUserSig.SDKAPPID, userId, GenerateTestUserSig.genTestUserSig(userId),
                new TUICallback() {
                    @Override
                    public void onSuccess() {
                        Log.i(TAG, "login success");
                        AppStore.userId = userId;
                        getUserInfo(userId);
                    }

                    @Override
                    public void onError(int errorCode, String errorMessage) {
                        ToastUtil.toastShortMessage(getString(R.string.app_toast_login_fail));
                        Log.e(TAG, "login fail，error code: " + errorCode + " message:" + errorMessage);
                    }
                });
    }

    private void getUserInfo(String userId) {
        List<String> userList = new ArrayList<>();
        userList.add(userId);

        V2TIMManager v2TIMManager = V2TIMManager.getInstance();
        v2TIMManager.getUsersInfo(userList, new V2TIMValueCallback<List<V2TIMUserFullInfo>>() {
            @Override
            public void onError(int errorCode, String errorMsg) {
                Log.e(TAG, "getUserInfo failed, code:" + errorCode + " msg: " + errorMsg);
            }

            @Override
            public void onSuccess(List<V2TIMUserFullInfo> userFullInfoList) {
                if (userFullInfoList == null || userFullInfoList.isEmpty()) {
                    Log.w(TAG, "getUserInfo result is empty");
                    return;
                }
                V2TIMUserFullInfo timUserFullInfo = userFullInfoList.get(0);
                String userName = timUserFullInfo.getNickName();
                String userAvatar = timUserFullInfo.getFaceUrl();
                Log.d(TAG, "getUserInfo success: userName = " + userName + " , userAvatar = " + userAvatar);

                if (TextUtils.isEmpty(userName) || TextUtils.isEmpty(userAvatar)) {
                    startProfileActivity();
                } else {
                    AppStore.userAvatar = userAvatar;
                    AppStore.userName = userName;
                    startMainActivity();
                }
                finish();
            }
        });
    }

    private void startProfileActivity() {
        Intent intent = new Intent(LoginActivity.this, ProfileActivity.class);
        startActivity(intent);
    }

    private void startMainActivity() {
        Intent intent = new Intent(LoginActivity.this, MainActivity.class);
        startActivity(intent);
    }
}
