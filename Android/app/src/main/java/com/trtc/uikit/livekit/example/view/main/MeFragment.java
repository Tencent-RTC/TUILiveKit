package com.trtc.uikit.livekit.example.view.main;

import android.os.Bundle;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.service.ICallBack;
import com.trtc.uikit.livekit.example.service.IMManager;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.example.view.modify.ChangeNickNamePicker;


public class MeFragment extends Fragment {
    private ImageView            mImageAvatar;
    private TextView             mTextNickname;
    private TextView             mTextMyFollowCount;
    private TextView             mTextFollowMeCount;
    private ChangeNickNamePicker mPicker;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.app_fragment_me, container, false);
        bindViewId(contentView);
        initView();
        initDate();
        return contentView;
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
                if (!isAdded() || requireActivity().isDestroyed() || requireActivity().isFinishing()) {
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
            mPicker = new ChangeNickNamePicker(getContext(), mTextNickname);
            if (!mPicker.isShowing()) {
                mPicker.show();
            }
        });
    }

    private void initAvatarView() {
        ImageLoader.load(getContext(),
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

    private void bindViewId(View contentView) {
        mTextNickname = contentView.findViewById(R.id.tv_nickname);
        mTextMyFollowCount = contentView.findViewById(R.id.tv_likes_count);
        mTextFollowMeCount = contentView.findViewById(R.id.tv_follows_count);
        mImageAvatar = contentView.findViewById(R.id.iv_avatar);
    }
}