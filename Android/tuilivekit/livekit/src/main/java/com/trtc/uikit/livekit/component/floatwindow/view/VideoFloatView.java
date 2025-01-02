package com.trtc.uikit.livekit.component.floatwindow.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.anchor.floatwindow.FloatViewAnchorView;
import com.trtc.uikit.livekit.livestream.view.audience.floatwindow.FloatViewAudienceView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class VideoFloatView extends FrameLayout {

    private View      mCloseView;
    private ImageView mEnableVolume;
    private ImageView mEnableMic;
    private ImageView mEnableVideo;

    private final Context           mContext;
    private final LiveStreamManager mLiveStreamManager;
    private final LiveCoreView      mCoreView;

    private final Observer<TUIRoomDefine.Role> mRoleObserver            = this::onRoleChanged;
    private final Observer<Boolean>            mMicrophoneMutedObserver = this::onMicrophoneMuted;
    private final Observer<Boolean>            mCameraOpenedObserver    = this::onCameraOpened;

    public VideoFloatView(@NonNull Context context, LiveStreamManager liveStreamManager, LiveCoreView coreView) {
        super(context);
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        mCoreView = coreView;
        initView();
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_video_float_layout, this);
        mCloseView = findViewById(R.id.iv_close);
        mEnableVolume = findViewById(R.id.iv_enable_volume);
        mEnableMic = findViewById(R.id.iv_enable_mic);
        mEnableVideo = findViewById(R.id.iv_enable_video);

        FrameLayout frameLayout = findViewById(R.id.fl_video_grid_container);
        if (isOwner()) {
            FloatViewAnchorView videoView = new FloatViewAnchorView(mContext, mCoreView);
            videoView.init(mLiveStreamManager);
            frameLayout.addView(videoView);
        } else {
            FloatViewAudienceView videoView = new FloatViewAudienceView(mContext, mCoreView);
            videoView.init(mLiveStreamManager);
            frameLayout.addView(videoView);
        }
        mCloseView.setOnClickListener(v -> FloatWindowManager.getInstance().releaseFloatWindow());
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mLiveStreamManager.getUserState().selfInfo.role.observe(mRoleObserver);
        mLiveStreamManager.getMediaState().isMicrophoneMuted.observe(mMicrophoneMutedObserver);
        mLiveStreamManager.getMediaState().isCameraOpened.observe(mCameraOpenedObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveStreamManager.getUserState().selfInfo.role.removeObserver(mRoleObserver);
        mLiveStreamManager.getMediaState().isMicrophoneMuted.removeObserver(mMicrophoneMutedObserver);
        mLiveStreamManager.getMediaState().isCameraOpened.removeObserver(mCameraOpenedObserver);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        if (ev.getAction() == MotionEvent.ACTION_MOVE) {
            return true;
        }
        return super.onInterceptTouchEvent(ev);
    }

    private boolean isOwner() {
        LiveStreamManager liveStreamManager = mLiveStreamManager;
        if (liveStreamManager != null) {
            TUIRoomDefine.Role role = liveStreamManager.getUserState().selfInfo.role.get();
            return role == TUIRoomDefine.Role.ROOM_OWNER;
        }
        return false;
    }

    private void onRoleChanged(TUIRoomDefine.Role role) {
        if (role == TUIRoomDefine.Role.GENERAL_USER) {
            mEnableVolume.setVisibility(VISIBLE);
            mEnableMic.setVisibility(GONE);
            mEnableVideo.setVisibility(GONE);
        } else {
            mEnableVolume.setVisibility(GONE);
            mEnableMic.setVisibility(VISIBLE);
            mEnableVideo.setVisibility(VISIBLE);
        }
    }

    private void onMicrophoneMuted(boolean muted) {
        if (muted) {
            mEnableMic.setImageResource(R.drawable.livekit_microphone_closed);
        } else {
            mEnableMic.setImageResource(R.drawable.livekit_microphone_opened);
        }
    }

    private void onCameraOpened(boolean opened) {
        if (opened) {
            mEnableVideo.setImageResource(R.drawable.livekit_video_opened);
        } else {
            mEnableVideo.setImageResource(R.drawable.livekit_video_closed);
        }
    }
}
