package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.widgets;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatFullInfo;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;

public class CoGuestBackgroundWidgetsView extends BasicView {

    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("coGuest-BackgroundWidgetsView");

    private       SeatFullInfo      mState                    = new SeatFullInfo();
    private       ImageFilterView   mImageAvatar;
    private final Observer<Boolean> mPictureInPictureObserver = this::onPictureInPictureObserver;

    public CoGuestBackgroundWidgetsView(@NonNull Context context) {
        this(context, null);
    }

    public CoGuestBackgroundWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoGuestBackgroundWidgetsView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AudienceManager manager, TUIRoomDefine.SeatFullInfo userInfo) {
        LOGGER.info("init userId:" + userInfo.userId);
        mState = userInfo;
        super.init(manager);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_background_widgets_view, this, true);
        mImageAvatar = findViewById(R.id.iv_avatar);
    }

    @Override
    public void refreshView() {
        ImageLoader.load(mContext, mImageAvatar, mState.userAvatar, R.drawable.livekit_ic_avatar);
    }

    @Override
    protected void addObserver() {
        mMediaState.isPictureInPictureMode.observeForever(mPictureInPictureObserver);
    }

    @Override
    protected void removeObserver() {
        mMediaState.isPictureInPictureMode.removeObserver(mPictureInPictureObserver);
    }

    private void onPictureInPictureObserver(Boolean isPipMode) {
        if (Boolean.TRUE.equals(isPipMode)) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
