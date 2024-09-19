package com.trtc.uikit.livekit.example.view.me;

import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.service.ICallBack;
import com.trtc.uikit.livekit.example.service.IMManager;
import com.trtc.uikit.livekit.example.store.AppStore;


public class MeActivity extends FullScreenActivity {
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
        initNickNameView();
        initMyFollowCountView();
        initFollowMeCountView();
        initAvatarView();
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
        mTextNickname = findViewById(R.id.tv_nickname);
        mTextMyFollowCount = findViewById(R.id.tv_likes_count);
        mTextFollowMeCount = findViewById(R.id.tv_follows_count);
        mImageAvatar = findViewById(R.id.iv_avatar);
    }
}