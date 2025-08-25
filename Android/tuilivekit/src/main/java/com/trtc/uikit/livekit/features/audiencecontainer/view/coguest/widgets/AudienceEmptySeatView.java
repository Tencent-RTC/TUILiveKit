package com.trtc.uikit.livekit.features.audiencecontainer.view.coguest.widgets;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatFullInfo;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.view.BasicView;

public class AudienceEmptySeatView extends BasicView {

    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("coGuest-AudienceEmptySeatView");

    private       SeatFullInfo      mSeatInfo;
    private final Observer<Boolean> mPictureInPictureObserver = this::onPictureInPictureObserver;

    public AudienceEmptySeatView(@NonNull Context context) {
        this(context, null);
    }

    public AudienceEmptySeatView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceEmptySeatView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AudienceManager manager, SeatFullInfo seatInfo) {
        LOGGER.info("init seatInfo:" + new Gson().toJson(seatInfo));
        mSeatInfo = seatInfo;
        super.init(manager);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_empty_audience_widgets_view, this, true);
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
