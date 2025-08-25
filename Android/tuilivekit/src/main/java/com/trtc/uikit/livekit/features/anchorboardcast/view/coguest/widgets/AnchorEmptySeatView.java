package com.trtc.uikit.livekit.features.anchorboardcast.view.coguest.widgets;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.AnchorManager;
import com.trtc.uikit.livekit.features.anchorboardcast.view.BasicView;

public class AnchorEmptySeatView extends BasicView {

    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("coGuest-AnchorEmptySeatView");

    private       TUIRoomDefine.SeatFullInfo mSeatInfo;
    private       TextView                   mTextSeatIndex;
    private final Observer<Boolean>          mPipModeObserver = this::onPipModeObserver;

    public AnchorEmptySeatView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorEmptySeatView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorEmptySeatView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void init(AnchorManager manager, TUIRoomDefine.SeatFullInfo seatInfo) {
        LOGGER.info("init seatInfo:" + new Gson().toJson(seatInfo));
        mSeatInfo = seatInfo;
        super.init(manager);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_empty_anchor_widgets_view, this, true);
        mTextSeatIndex = findViewById(R.id.tv_seat_index);
    }

    @Override
    protected void refreshView() {
        if (mSeatInfo != null) {
            String seatIndex = mSeatInfo.seatIndex + "";
            mTextSeatIndex.setText(seatIndex);
        }
    }

    @Override
    protected void addObserver() {
        mMediaState.isPipModeEnabled.observeForever(mPipModeObserver);
    }

    @Override
    protected void removeObserver() {
        mMediaState.isPipModeEnabled.removeObserver(mPipModeObserver);
    }

    private void onPipModeObserver(Boolean isPipMode) {
        if (Boolean.TRUE.equals(isPipMode)) {
            setVisibility(GONE);
        } else {
            setVisibility(VISIBLE);
        }
    }
}
