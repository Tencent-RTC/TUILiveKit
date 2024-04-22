package com.trtc.uikit.livekit.liveroom.view.common.video;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

@SuppressLint("ViewConstructor")
public class VideoView extends FrameLayout {

    protected     UserInfo          mUserInfo;
    protected     Context           mContext;
    protected     LiveRoomInfo      mLiveRoomInfo;
    protected     ImageView         mImageEnableAudio;
    protected     TextView          mTextName;
    protected     ImageView         mImageAvatar;
    protected     FrameLayout       mLayoutRoot;
    protected     TUIVideoView      mTUIVideoView;
    protected     RoomEngineService mRoomEngineService;
    private final Observer<Boolean> mCameraOpenObserver = (isOpen) -> refreshView();
    private final Observer<Boolean> mMuteAudioObserver  = (muteAudio) ->
            mImageEnableAudio.setVisibility(muteAudio ? VISIBLE : GONE);

    public VideoView(@NonNull Context context, LiveRoomInfo roomInfo, UserInfo userInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mUserInfo = userInfo;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
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
        mUserInfo.videoInfo.isCameraOpened.observe(mCameraOpenObserver);
        mUserInfo.audioInfo.muteAudio.observe(mMuteAudioObserver);
    }

    private void removeObserver() {
        mUserInfo.videoInfo.isCameraOpened.removeObserver(mCameraOpenObserver);
        mUserInfo.audioInfo.muteAudio.removeObserver(mMuteAudioObserver);
    }

    private void init() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_video_view, this, true);
        mLayoutRoot = findViewById(R.id.fl_root);
        mTextName = findViewById(R.id.tv_name);
        mImageEnableAudio = findViewById(R.id.iv_enable_audio);
        mTUIVideoView = findViewById(R.id.tui_video_view);
        mImageAvatar = findViewById(R.id.iv_avatar);

        refreshView();
    }

    public TUIVideoView getTUIVideoView() {
        return mTUIVideoView;
    }

    public void clear() {
        if (mTUIVideoView != null) {
            mTUIVideoView = null;
        }
    }

    private void refreshView() {
        if (mUserInfo.videoInfo.isCameraOpened.get()) {
            mImageAvatar.setVisibility(GONE);
        } else {
            mImageAvatar.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageAvatar, mUserInfo.avatarUrl.get(), R.drawable.livekit_ic_avatar);
        }
    }

    @Override
    public void setBackgroundColor(int color) {
        mLayoutRoot.setBackgroundColor(color);
    }
}
