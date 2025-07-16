package com.trtc.uikit.livekit.voiceroom.view.bottommenu;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatManagerDialog;
import com.trtc.uikit.livekit.voiceroom.view.settings.SettingsDialog;

import java.util.LinkedHashSet;

public class AnchorFunctionView extends BasicView {
    private TextView          mSeatApplicationCountText;
    private SettingsDialog    mSettingsDialog;
    private SeatManagerDialog mSeatManagerDialog;

    private final Observer<LinkedHashSet<SeatState.SeatApplication>> mSeatApplicationListObserver =
            this::updateSeatApplicationCountText;

    public AnchorFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorFunctionView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_anchor_function, this, true);
        mSeatApplicationCountText = findViewById(R.id.application_count);
        findViewById(R.id.iv_settings).setOnClickListener(v -> showSettingsPanel());
        findViewById(R.id.iv_seat_management).setOnClickListener(v -> showSeatManagementPanel());
    }

    @Override
    protected void addObserver() {
        mSeatState.seatApplicationList.observeForever(mSeatApplicationListObserver);
    }

    @Override
    protected void removeObserver() {
        mSeatState.seatApplicationList.removeObserver(mSeatApplicationListObserver);
    }

    private void showSettingsPanel() {
        if (mSettingsDialog == null) {
            mSettingsDialog = new SettingsDialog(mContext, mVoiceRoomManager);
        }
        mSettingsDialog.show();
    }

    private void showSeatManagementPanel() {
        if (mSeatManagerDialog == null) {
            mSeatManagerDialog = new SeatManagerDialog(mContext, mVoiceRoomManager, mSeatGridView);
        }
        mSeatManagerDialog.show();
    }

    private void updateSeatApplicationCountText(LinkedHashSet<SeatState.SeatApplication> list) {
        if (list.isEmpty()) {
            mSeatApplicationCountText.setVisibility(INVISIBLE);
        } else {
            mSeatApplicationCountText.setVisibility(VISIBLE);
            mSeatApplicationCountText.setText(String.valueOf(list.size()));
        }
    }
}
