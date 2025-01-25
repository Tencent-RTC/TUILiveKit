package com.trtc.uikit.livekit.example.view.me;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.ui.ConfirmWithCheckboxDialog;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.service.ICallBack;
import com.trtc.uikit.livekit.example.service.IMManager;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.example.view.login.LoginActivity;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;


public class MeActivity extends BaseActivity {
    private ImageView            mImageLogout;
    private ImageView            mImageBack;
    private ImageView            mImageAvatar;
    private TextView             mTextNickname;
    private TextView             mTextMyFollowCount;
    private TextView             mTextFollowMeCount;
    private ChangeNickNamePicker mPicker;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_fragment_me);

        bindViewId();
        initView();
        initDate();
    }

    protected void initView() {
        initLogoutView();
        initBackView();
        initNickNameView();
        initMyFollowCountView();
        initFollowMeCountView();
        initAvatarView();
    }

    private void initLogoutView() {
        mImageLogout.setOnClickListener(v -> {
            showLogoutDialog();
        });
    }

    private void initBackView() {
        mImageBack.setOnClickListener(v -> {
            finish();
        });
    }

    private void initDate() {
        IMManager.getUserFollowInfo(AppStore.userId, new ICallBack() {
            @Override
            public void onSuccess() {
                if (isDestroyed() || isFinishing()) {
                    return;
                }
                mTextFollowMeCount.post(() -> {
                    mTextFollowMeCount.setText(String.valueOf(AppStore.fansCount));
                    mTextMyFollowCount.setText(String.valueOf(AppStore.followCount));
                });
            }

            @Override
            public void onError(int code, String desc) {

            }
        });
    }

    private void initNickNameView() {
        mTextNickname.setText(TUILogin.getNickName());
        mTextNickname.setOnClickListener(view -> {
            mPicker = new ChangeNickNamePicker(this, mTextNickname);
            if (!mPicker.isShowing()) {
                mPicker.show();
            }
        });
    }

    private void initAvatarView() {
        ImageLoader.load(getApplicationContext(),
                mImageAvatar,
                TUILogin.getFaceUrl(),
                com.trtc.uikit.livekit.R.drawable.livekit_ic_avatar);
    }

    private void initFollowMeCountView() {
        mTextFollowMeCount.setText(String.valueOf(AppStore.fansCount));
    }

    private void initMyFollowCountView() {
        mTextMyFollowCount.setText(String.valueOf(AppStore.followCount));
    }

    private void bindViewId() {
        mImageLogout = findViewById(R.id.iv_logout);
        mImageBack = findViewById(R.id.iv_back);
        mTextNickname = findViewById(R.id.tv_nickname);
        mTextMyFollowCount = findViewById(R.id.tv_likes_count);
        mTextFollowMeCount = findViewById(R.id.tv_follows_count);
        mImageAvatar = findViewById(R.id.iv_avatar);
    }

    private void showLogoutDialog() {
        ConfirmWithCheckboxDialog dialog = new ConfirmWithCheckboxDialog(this);
        dialog.setTitle(getString(R.string.app_tips_title));
        dialog.setContent(getString(R.string.app_logout_tips));
        dialog.setNegativeText(getString(R.string.app_cancel), negativeView -> dialog.dismiss());
        dialog.setPositiveText(getString(R.string.app_logout), positiveView -> {
            VideoLiveKit.createInstance(getApplicationContext()).stopLive(new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    TUILogin.logout(new TUICallback() {
                        @Override
                        public void onSuccess() {
                        }

                        @Override
                        public void onError(int errorCode, String errorMessage) {
                        }
                    });
                }

                @Override
                public void onError(TUICommonDefine.Error error, String s) {
                }
            });

            Intent intent = new Intent(MeActivity.this, LoginActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(intent);
            finish();
        });
        dialog.show();


    }


}