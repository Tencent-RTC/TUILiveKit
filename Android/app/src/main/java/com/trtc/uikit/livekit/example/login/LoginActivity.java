package com.trtc.uikit.livekit.example.login;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.WindowManager;
import android.widget.EditText;

import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.imsdk.v2.V2TIMValueCallback;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.tencent.qcloud.tuikit.debug.GenerateTestUserSig;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.main.MainActivity;
import com.trtc.uikit.livekit.example.settings.SettingsConfig;

import java.util.ArrayList;
import java.util.List;

public class LoginActivity extends BaseActivity {
    private static final String TAG = "LoginActivity";

    private EditText mEditUserId;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_login);
        getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);

        mEditUserId = findViewById(R.id.et_userId);

        findViewById(R.id.btn_login).setOnClickListener(v -> {
            String userId = mEditUserId.getText().toString().trim();
            login(userId);
        });
    }

    private void login(String userId) {
        if (TextUtils.isEmpty(userId)) {
            ToastUtil.toastShortMessage(getString(R.string.app_user_id_is_empty));
            return;
        }

        TUILogin.login(this, GenerateTestUserSig.SDKAPPID, userId, GenerateTestUserSig.genTestUserSig(userId),
                new TUICallback() {
                    @Override
                    public void onSuccess() {
                        Log.i(TAG, "login success");
                        SettingsConfig.userId = userId;
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
                    SettingsConfig.userAvatar = userAvatar;
                    SettingsConfig.userName = userName;
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
