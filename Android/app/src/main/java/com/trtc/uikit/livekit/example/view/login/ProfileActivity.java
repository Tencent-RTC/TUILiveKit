package com.trtc.uikit.livekit.example.view.login;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import com.tencent.imsdk.v2.V2TIMCallback;
import com.tencent.imsdk.v2.V2TIMManager;
import com.tencent.imsdk.v2.V2TIMUserFullInfo;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.service.ICallBack;
import com.trtc.uikit.livekit.example.service.IMManager;
import com.trtc.uikit.livekit.example.view.main.MainActivity;
import com.trtc.uikit.livekit.example.store.AppStore;

import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ProfileActivity extends BaseActivity {
    private static final String TAG = "ProfileActivity";

    private ImageView mImageAvatar;
    private EditText  mEditUserName;
    private Button    mButtonRegister;
    private TextView  mTextInputTips;

    private String mAvatarUrl;

    private static final String[] USER_AVATAR_ARRAY = {
            "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png",
            "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
            "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar3.png",
            "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar4.png",
            "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar5.png"
    };

    private static final int[] CUSTOM_NAME_ARRAY = {
            R.string.app_custom_name_1,
            R.string.app_custom_name_2,
            R.string.app_custom_name_3,
            R.string.app_custom_name_4,
            R.string.app_custom_name_5,
    };


    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_login_profile);
        initAvatarImageView();
        initUserEditText();
        initRegisterButton();
    }

    private void initAvatarImageView() {
        mImageAvatar = findViewById(R.id.iv_user_avatar);
        int index = new Random().nextInt(USER_AVATAR_ARRAY.length);
        mAvatarUrl = USER_AVATAR_ARRAY[index];
        ImageLoader.load(this, mImageAvatar, mAvatarUrl, R.drawable.app_avatar);
    }

    private void initUserEditText() {
        mEditUserName = findViewById(R.id.et_user_name);
        mTextInputTips = findViewById(R.id.tv_tips_user_name);

        int index = new Random().nextInt(CUSTOM_NAME_ARRAY.length);
        String userName = getString(CUSTOM_NAME_ARRAY[index]);

        mEditUserName.setText(userName);
        mEditUserName.setSelection(userName.length());
        mEditUserName.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence text, int start, int count, int after) {
            }

            @Override
            public void onTextChanged(CharSequence text, int start, int before, int count) {
                if (isUserNameValid(text)) {
                    mTextInputTips.setTextColor(getResources().getColor(R.color.app_text_color_hint));
                } else {
                    mTextInputTips.setTextColor(getResources().getColor(R.color.app_color_input_no_match));
                }
                mButtonRegister.setEnabled(text.length() != 0);
            }

            @Override
            public void afterTextChanged(Editable s) {
            }
        });
    }

    private void initRegisterButton() {
        mButtonRegister = findViewById(R.id.tv_register);
        mButtonRegister.setOnClickListener(v -> setSelfInfo());

    }

    private void setSelfInfo() {
        String userName = mEditUserName.getText().toString().trim();
        if (TextUtils.isEmpty(userName)) {
            ToastUtil.toastLongMessage(getString(R.string.app_hint_user_name));
            return;
        }
        IMManager.setSelfInfo(mAvatarUrl, userName, new ICallBack() {
            @Override
            public void onSuccess() {
                ToastUtil.toastLongMessage(getString(R.string.app_toast_register_success_and_logging_in));
                AppStore.userName = userName;
                AppStore.userAvatar = mAvatarUrl;
                startMainActivity();
                finish();
            }

            @Override
            public void onError(int code, String message) {
                ToastUtil.toastLongMessage(getString(R.string.app_toast_failed_to_set, message));
                startMainActivity();
                finish();
            }
        });
    }

    private boolean isUserNameValid(CharSequence text) {
        String editable = text.toString();
        Pattern pattern = Pattern.compile("^[a-z0-9A-Z\\u4e00-\\u9fa5\\_]{2,20}$");
        Matcher matcher = pattern.matcher(editable);
        return matcher.matches();
    }

    private void startMainActivity() {
        Intent intent = new Intent(ProfileActivity.this, MainActivity.class);
        startActivity(intent);
    }
}
