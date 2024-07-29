package com.trtc.uikit.livekit.view.voiceroom.view.preview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;

import androidx.appcompat.widget.SwitchCompat;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BottomPanelView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class SettingsPanelView extends BottomPanelView {
    private SwitchCompat mSwitchCompat;

    private final Observer<TUIRoomDefine.SeatMode> mSeatModeObserver = this::updateSeatMode;

    public SettingsPanelView(Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        View rootView = LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_preview_settings, this, true);
        mSwitchCompat = findViewById(R.id.need_request);
        mSwitchCompat.setOnCheckedChangeListener((compoundButton, enable) -> onSeatModeClicked(enable));
        ImageView imageBack = rootView.findViewById(R.id.iv_back);
        imageBack.setOnClickListener(view -> onBackButtonClick());
    }

    @Override
    protected void addObserver() {
        mRoomState.seatMode.observe(mSeatModeObserver);
    }

    @Override
    protected void removeObserver() {
        mRoomState.seatMode.removeObserver(mSeatModeObserver);
    }

    private void onSeatModeClicked(boolean enable) {
        mLiveController.getRoomController().updateRoomSeatMode(enable
                ? TUIRoomDefine.SeatMode.APPLY_TO_TAKE : TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    }

    private void updateSeatMode(TUIRoomDefine.SeatMode seatMode) {
        mSwitchCompat.setChecked(seatMode == TUIRoomDefine.SeatMode.APPLY_TO_TAKE);
    }
}

