package com.trtc.uikit.livekit.view.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.SeatState;

import java.util.LinkedHashSet;

@SuppressLint("ViewConstructor")
public class VideoView extends BasicView {

    protected SeatState.SeatInfo mSeatInfo;
    protected ImageView          mImageEnableAudio;
    protected TextView           mTextName;
    protected ImageView          mImageAvatar;
    protected FrameLayout        mLayoutRoot;
    protected TUIVideoView       mTUIVideoView;

    private final Observer<LinkedHashSet<String>> hasVideoStreamUserListObserver = this::onVideoStreamUserListChange;
    private final Observer<LinkedHashSet<String>> hasAudioStreamUserListObserver = this::onAudioStreamUserListChange;

    public VideoView(@NonNull Context context, LiveController liveController, SeatState.SeatInfo seatInfo) {
        super(context, liveController);
        mSeatInfo = seatInfo;
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveController.getVideoViewFactory().removeVideoViewByUserId(mSeatInfo.userId.get());
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_video_view, this, true);
        bindViewId();

        initImageAvatar();
        initImageEnableAudio();
    }

    @Override
    protected void addObserver() {
        mUserState.hasVideoStreamUserList.observe(hasVideoStreamUserListObserver);
        mUserState.hasAudioStreamUserList.observe(hasAudioStreamUserListObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.hasVideoStreamUserList.removeObserver(hasVideoStreamUserListObserver);
        mUserState.hasAudioStreamUserList.removeObserver(hasAudioStreamUserListObserver);
    }

    public TUIVideoView getTUIVideoView() {
        return mTUIVideoView;
    }

    public void clear() {
        if (mTUIVideoView != null) {
            mTUIVideoView = null;
        }
    }

    @Override
    public void setBackgroundColor(int color) {
        mLayoutRoot.setBackgroundColor(color);
    }

    private void bindViewId() {
        mLayoutRoot = findViewById(R.id.fl_root);
        mTextName = findViewById(R.id.tv_name);
        mImageEnableAudio = findViewById(R.id.iv_enable_audio);
        mTUIVideoView = findViewById(R.id.tui_video_view);
        mImageAvatar = findViewById(R.id.iv_avatar);
    }

    protected void initImageAvatar() {
        String userId = mSeatInfo.userId.get();
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        boolean hasVideoStream = mUserState.hasVideoStreamUserList.get().contains(userId);
        boolean isPreview = LiveDefine.LiveStatus.PREVIEWING == mViewState.liveStatus.get();
        if (isPreview || hasVideoStream) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mSeatInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
        }
    }

    protected void initImageEnableAudio() {
        String userId = mSeatInfo.userId.get();
        if (TextUtils.isEmpty(userId)) {
            return;
        }
        mImageEnableAudio.setVisibility(mUserState.hasAudioStreamUserList.get().contains(userId) ? GONE : VISIBLE);
    }

    private void onVideoStreamUserListChange(LinkedHashSet<String> strings) {
        initImageAvatar();
    }

    private void onAudioStreamUserListChange(LinkedHashSet<String> strings) {
        initImageEnableAudio();
    }
}
