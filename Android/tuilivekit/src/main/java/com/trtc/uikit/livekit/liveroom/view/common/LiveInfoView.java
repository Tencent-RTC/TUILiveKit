package com.trtc.uikit.livekit.liveroom.view.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.android.material.imageview.ShapeableImageView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class LiveInfoView extends LinearLayout {

    private final LiveRoomInfo       mLiveRoomInfo;
    private       Context            mContext;
    private       ShapeableImageView mImageAvatar;
    private       TextView           mTextLiveName;
    private       PopupDialog        mStreamInfoDialog;
    private       StreamInfoPanel    mStreamInfoPanel;
    private final Observer<String>   mUserNameObserver   = (name) -> mTextLiveName.setText(name);
    private final Observer<String>   mUserAvatarObserver = (avatar) ->
            ImageLoader.load(mContext, mImageAvatar, avatar, R.drawable.livekit_ic_avatar);

    public LiveInfoView(Context context, LiveRoomInfo roomInfo) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        init();
    }

    private void init() {
        View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_live_info, this, true);
        mImageAvatar = rootView.findViewById(R.id.iv_avatar);
        mTextLiveName = rootView.findViewById(R.id.tv_live_name);
        mTextLiveName.setText(mLiveRoomInfo.name.get());
        ImageLoader.load(mContext, mImageAvatar, mLiveRoomInfo.anchorInfo.avatarUrl.get(),
                R.drawable.livekit_ic_avatar);

        rootView.setOnClickListener(view -> {
            if (mStreamInfoDialog == null) {
                mStreamInfoDialog = new PopupDialog(mContext);
                mStreamInfoDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mStreamInfoPanel == null) {
                mStreamInfoPanel = new StreamInfoPanel(mContext, mLiveRoomInfo);
            } else {
                mStreamInfoPanel.updateStreamInfo(mLiveRoomInfo);
            }
            mStreamInfoDialog.setView(mStreamInfoPanel);
            mStreamInfoDialog.show();
        });
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mLiveRoomInfo.name.observe(mUserNameObserver);
        mLiveRoomInfo.anchorInfo.avatarUrl.observe(mUserAvatarObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.name.removeObserver(mUserNameObserver);
        mLiveRoomInfo.anchorInfo.avatarUrl.removeObserver(mUserAvatarObserver);
    }
}