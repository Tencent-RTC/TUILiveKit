package com.trtc.uikit.livekit.voiceroom.view.topview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.google.android.material.imageview.ShapeableImageView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.view.BasicView;

@SuppressLint("ViewConstructor")
public class RoomInfoView extends BasicView {
    private TextView           mOwnerName;
    private ShapeableImageView mImageAvatar;

    private final Observer<String> mOwnerNameObserver = name -> mOwnerName.setText(name);

    private final Observer<String> mOwnerAvatarObserver = (avatar) -> ImageLoader.load(mContext, mImageAvatar, avatar
            , R.drawable.livekit_ic_avatar);

    public RoomInfoView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_layout_anchor_live_info, this, true);
        mOwnerName = findViewById(R.id.tv_live_name);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mOwnerName.setText(mUserState.ownerInfo.name.get());
        ImageLoader.load(mContext, mImageAvatar, mUserState.ownerInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
    }

    @Override
    protected void addObserver() {
        mUserState.ownerInfo.name.observe(mOwnerNameObserver);
        mUserState.ownerInfo.avatarUrl.observe(mOwnerAvatarObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.ownerInfo.name.removeObserver(mOwnerNameObserver);
        mUserState.ownerInfo.avatarUrl.removeObserver(mOwnerAvatarObserver);
    }
}
